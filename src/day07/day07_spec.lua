local day07 = require("day07")

describe("day07", function()
    it("solves the example", function()
        local input = [[190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
]]
        local part1, part2 = day07.solve(input)
        assert.are.equal(3749, part1)
        assert.are.equal(0, part2)
    end)
end)
