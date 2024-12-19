local day19 = require("day19")

describe("day19", function()
    describe("example 1", function()
        local input = [[r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
]]
        local part1, part2 = day19.solve(input)
        it("part1", function()
            assert.are.equal(6, part1)
        end)
        it("part2", function()
            assert.are.equal(0, part2)
        end)
    end)
end)
