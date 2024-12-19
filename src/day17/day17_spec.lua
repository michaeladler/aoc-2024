local day17 = require("day17")

describe("day17", function()
    describe("example 1", function()
        local input = [[Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
]]
        local part1, _ = day17.solve(input)
        it("part1", function()
            assert.are.equal("4,6,3,5,6,3,5,2,1,0", part1)
        end)
    end)

    describe("example 2", function()
        local input = [[Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
]]
        local _, part2 = day17.solve(input)
        it("part2", function()
            assert.are.equal(117440, part2)
        end)
    end)
end)
