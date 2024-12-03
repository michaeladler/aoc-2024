local day03 = require("day03")

describe("day03", function()
    it("solves example 1", function()
        local input = [[xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
]]
        local part1, _ = day03.solve(input)
        assert.are.equal(161, part1)
    end)
    it("solves example 2", function()
        local input = [[xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
]]
        local _, part2 = day03.solve(input)
        assert.are.equal(48, part2)
    end)
end)
