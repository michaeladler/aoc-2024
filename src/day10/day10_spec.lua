local day10 = require("day10")

describe("day10", function()
    it("solves example 1", function()
        local input = [[0123
1234
8765
9876
]]
        local part1, _ = day10.solve(input)
        assert.are.equal(1, part1)
    end)

    it("solves example 2", function()
        local input = [[89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
]]
        local part1, part2 = day10.solve(input)
        assert.are.equal(36, part1)
        assert.are.equal(81, part2)
    end)
end)
