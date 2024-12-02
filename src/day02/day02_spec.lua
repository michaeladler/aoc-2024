local day02 = require("day02")

describe("day02", function()
    it("solves the example", function()
        local input = [[7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
]]
        local part1, part2 = day02.solve(input)
        assert.are.equal(2, part1)
        assert.are.equal(4, part2)
    end)
end)
