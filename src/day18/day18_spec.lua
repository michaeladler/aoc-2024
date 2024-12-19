local day18 = require("day18")

describe("day18", function()
    describe("example", function()
        local input = [[5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
]]
        local points = day18.parse(input)
        local part1, part2 = day18.shortest_path(points, 6, 12)
        it("part1", function()
            assert.are.equal(22, part1)
        end)
        it("part2", function()
            assert.are.equal("6,1", part2)
        end)
    end)
end)
