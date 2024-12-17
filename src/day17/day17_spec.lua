local day17 = require("day17")

describe("day17", function()
    describe("example 1", function()
        local input = [[Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
]]
        local part1, part2 = day17.solve(input)
        it("part1", function()
            assert.are.equal("4,6,3,5,6,3,5,2,1,0", part1)
        end)
        it("part2", function()
            assert.are.equal(0, part2)
        end)
    end)
end)
