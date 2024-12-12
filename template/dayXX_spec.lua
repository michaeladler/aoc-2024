local dayXX = require("dayXX")

describe("dayXX", function()
    describe("example 1", function()
        local input = [[
]]
        local part1, part2 = dayXX.solve(input)
        it("part1", function()
            assert.are.equal(0, part1)
        end)
        it("part2", function()
            assert.are.equal(0, part2)
        end)
    end)
end)
