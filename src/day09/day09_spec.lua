local day09 = require("day09")

describe("day09", function()
    it("solves the example", function()
        local input = "2333133121414131402\n"
        local part1, part2 = day09.solve(input)
        assert.are.equal(1928, part1)
        assert.are.equal(2858, part2)
    end)
end)
