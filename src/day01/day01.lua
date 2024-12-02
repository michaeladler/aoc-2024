-- Author: Michael Adler
local M = {}

M.solve = function(input)
	local t1, t2 = {}, {}
	local count = {}
	for line in input:gmatch("[^\r\n]+") do
		local num1, num2 = line:match("(%d+)%s+(%d+)")
		local x, y = tonumber(num1), tonumber(num2)
		table.insert(t1, x)
		table.insert(t2, y)
		count[y] = (count[y] or 0) + 1
	end

	table.sort(t1)
	table.sort(t2)

	local part1, part2 = 0, 0
	for i = 1, #t1 do
		local x, y = t1[i], t2[i]
		part1 = part1 + math.abs(x - y)
		part2 = part2 + x * (count[x] or 0)
	end
	return part1, part2
end

return M
