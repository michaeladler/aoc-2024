local day11 = require("day11")

describe("day11", function()
    it("solves the example", function()
        local input = "125 17\n"
        local part1, part2 = day11.solve(input)
        assert.are.equal(55312, part1)
        assert.are.equal(65601038650482, part2)
    end)
end)
