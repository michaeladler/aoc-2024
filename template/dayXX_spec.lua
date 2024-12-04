local dayXX = require("dayXX")

describe("dayXX", function()
    it("solves the example", function()
        local input = [[
]]
        local part1, part2 = dayXX.solve(input)
        assert.are.equal(0, part1)
        assert.are.equal(0, part2)
    end)
end)
