local day04 = require("day04")

describe("day04", function()
    it("solves the example", function()
        local input = [[MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
]]
        local part1, part2 = day04.solve(input)
        assert.are.equal(18, part1)
        assert.are.equal(9, part2)
    end)
end)
