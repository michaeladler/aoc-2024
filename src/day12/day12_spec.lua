local day12 = require("day12")

describe("day12", function()
    describe("example 1", function()
        local input = [[AAAA
BBCD
BBCC
EEEC
]]
        local part1, part2 = day12.solve(input)
        it("part1", function()
            assert.are.equal(140, part1)
        end)
        it("part2", function()
            assert.are.equal(80, part2)
        end)
    end)

    describe("example 2", function()
        local input = [[OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
]]
        local part1, part2 = day12.solve(input)
        it("part1", function()
            assert.are.equal(772, part1)
        end)
        it("part2", function()
            assert.are.equal(436, part2)
        end)
    end)

    describe("example 3", function()
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
        it("part1", function()
            assert.are.equal(1930, part1)
        end)
        it("part2", function()
            assert.are.equal(1206, part2)
        end)
    end)

    describe("example 4", function()
        local input = [[EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
]]
        local _, part2 = day12.solve(input)
        it("part2", function()
            assert.are.equal(236, part2)
        end)
    end)

    describe("example 5", function()
        local input = [[AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
]]
        local _, part2 = day12.solve(input)
        it("part2", function()
            assert.are.equal(368, part2)
        end)
    end)
end)
