module OrgBabel
using DataFrames, CSV, Tables

function write_message(stream::IO, message::String)
    try
        write(stream, message)
    catch file_err
        print("Could not write error file: $file_err")
    end
end

struct NameIsNotCallable <: Exception end

# TODO(lyterk): Use this to access memoized values, instead of re-evaluating
# every block in the file.
struct CallMap
    callables::Dict{String,Function}
    results::Dict{String,Any}
    CallMap(callables, results) = new(callables, results)
end

function assign(o::CallMap, name::String, lambda::Function)
    o.callables[name] = lambda
end

function call(o::CallMap, name::String)
    try
        callable = o.callables[name]
        result = callable()
        o.results[name] = result
        return result
    catch
        throw(NameIsNotCallable)
    end
end

function fetch_result(o::CallMap, name::String)
    try
        return o.results[name]
    catch
        throw(NameIsNotCallable)
    end
end

function CallMap()
    callables = Dict{String,Function}()
    results = Dict{String,Any}()
    CallMap(callables, results)
end

callmap = CallMap()

function write_result(stream::IO, result::Any)
    try
        if typeof(result) <: DataFrames.DataFrame
            CSV.write(stream,
                result,
                missingstring="",
                quotestrings=false)
        elseif typeof(result) <: Matrix
            CSV.write(stream, Tables.table(result))
        elseif typeof(result) <: Tables.MatrixTable
            # Conversion to Tables.MatrixTable has already happened.
            CSV.write(stream, Tables.table(result))
        else
            write_message(stream, string(result))
        end

        result
    catch file_error
        err_msg = "Failed to write to file, $file_error"
        write(stream, err_msg)

        err_msg
    end
end

function isolation_wrapper(name::String, user_input::String)
    # This is a super ugly way of handling this, but there really isn't a way to
    # parse then execute a macro of untrusted string without elevating that code
    # to :toplevel (which would lose the intended effect of not persisting
    # varibles when not in a session)
    return """
    function $name()
        $user_input
    end

    $name()
    """
end

# TODO: Make a wrapper to make this callable from elisp.
function internal_execute_julia(input_file::String, output::IO, is_in_session::Bool)
    result = try
        # The file contains the code from each discrete block.
        if is_in_session
            result = include(input_file)
        else
            name = hash(input_file)
            # Contain in a lambda to restrict the variable scope.
            evalable = open(input_file, "r") do f
                user_string = read(f, String)
                eval_string = isolation_wrapper(name, user_string)
                # TODO(lyterk): Use parseall to intercept invalid code?
                # Meta.parseall(eval_string)
            end

            lambda = () -> eval(evalable)
            assign(callmap, name, lambda)
            result = call(callmap, name)
            result
        end
        result
    catch user_err
        err_msg = "Source block evaluation failed: $user_err"
        write_message(output, err_msg)
        return err_msg
    end

    write_result(output, result)
end
end
