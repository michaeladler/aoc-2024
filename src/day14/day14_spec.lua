local day14 = require("day14")

describe("day14", function()
    describe("example 1", function()
        local input = [[p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
]]
        local part1, part2 = day14.solve(input, 11, 7)
        it("part1", function()
            assert.are.equal(12, part1)
        end)
        it("part2", function()
            assert.are.equal(0, part2)
        end)
    end)
end)
