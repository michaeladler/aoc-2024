local day03 = require("day03")

describe("day03", function()
	it("solves the example", function()
		local input = [[xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
]]
		local part1, part2 = day03.solve(input)
		assert.are.equal(161, part1)
		-- assert.are.equal(4, part2)
	end)
end)
