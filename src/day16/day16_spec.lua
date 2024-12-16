local day16 = require("day16")

describe("day16", function()
    describe("example 1", function()
        local input = [[###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
]]
        local part1, part2 = day16.solve(input)
        it("part1", function()
            assert.are.equal(7036, part1)
        end)
        it("part2", function()
            assert.are.equal(45, part2)
        end)
    end)

    describe("example 2", function()
        local input = [[#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
]]
        local part1, part2 = day16.solve(input)
        it("part1", function()
            assert.are.equal(11048, part1)
        end)
        it("part2", function()
            assert.are.equal(64, part2)
        end)
    end)
end)
