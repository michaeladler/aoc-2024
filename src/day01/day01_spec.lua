local day01 = require("day01")

describe("day01", function()
	it("solves the example", function()
		local input = [[3   4
4   3
2   5
1   3
3   9
3   3
]]
		local part1, part2 = day01.solve(input)
		assert.are.equal(11, part1)
		assert.are.equal(31, part2)
	end)
end)
