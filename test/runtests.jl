using Test
using OrgBabel
using CSV, DataFrames, Tables

@testset "Org Babel Julia testing" begin
    sut = OrgBabel

    function flush_reset(stream::IOBuffer)::String
        return String(take!(stream))
    end

    io = IOBuffer()

    @testset "Writing results to files" begin
        @testset "Write basic result" begin
            sut.write_result(io, 5)
            @test flush_reset(io) == "5"

            sut.write_result(io, "hello there")
            @test flush_reset(io) == "hello there"
        end

        @testset "Write matrix" begin
            A = [
                1 2 3
                4 5 6
                7 8 9
            ]
            sut.write_result(io, A)
            expected = "Column1,Column2,Column3\n1,2,3\n4,5,6\n7,8,9\n"
            @test flush_reset(io) == expected
        end

        @testset "Writing DataFrames" begin
            df = CSV.read("resources/example.csv", DataFrame)
            sut.write_result(io, df)
            open("resources/example.csv", "r") do f
                expected = read(f, String)
                @test flush_reset(io) == expected
            end
        end
    end

    @testset "Executing Julia files" begin
        dir = "resources/input_files/"

        @testset "Not in session" begin
            @testset "Simple operation" begin
                result = sut.internal_execute_julia("addition", string(dir, "addition.jl"), io, false)
                @test result == 2
            end

            @testset "Define and attempt to reference variable" begin
                definition = sut.internal_execute_julia("definition", string(dir, "defineVariable.jl"), io, false)
                @test definition == "hello there"

                reference = sut.internal_execute_julia("reference", string(dir, "referenceVariable.jl"), io, false)
                @test reference == "Source block evaluation failed: OrgBabel.NameIsNotCallable"
            end
        end

        @testset "In session" begin
            @testset "Simple operation" begin
                result = sut.internal_execute_julia("addition", string(dir, "addition.jl"), io, true)
                @test result == 2
            end

            @testset "Define and attempt to reference variable" begin
                definition = sut.internal_execute_julia("definition", string(dir, "defineVariable.jl"), io, true)
                @test definition == "hello there"

                reference = sut.internal_execute_julia("reference", string(dir, "referenceVariable.jl"), io, true)
                @test reference == "hello there"
            end
        end

    end
end
