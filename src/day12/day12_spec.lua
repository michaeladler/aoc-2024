local day12 = require("day12")

describe("day12", function()
    it("solves example 1", function()
        local input = [[AAAA
BBCD
BBCC
EEEC
]]
        local part1, part2 = day12.solve(input)
        assert.are.equal(140, part1)
        assert.are.equal(0, part2)
    end)

    it("solves example 2", function()
        local input = [[OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
]]
        local part1, part2 = day12.solve(input)
        assert.are.equal(772, part1)
        assert.are.equal(0, part2)
    end)

    it("solves example 3", function()
        local input = [[RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
]]
        local part1, part2 = day12.solve(input)
        assert.are.equal(1930, part1)
        assert.are.equal(0, part2)
    end)
end)
