generate DAY:
    #!/usr/bin/env bash
    set -euo pipefail
    DAY={{ DAY }}
    if [[ -d "src/day$DAY" ]]; then
        echo "day$DAY already exists"
        exit 1
    fi
    cp -a template "src/day$DAY"
    find "src/day$DAY" -type f -exec sed -i "s/dayXX/day$DAY/g" {} +
    cd "src/day$DAY"
    mv dayXX.lua "day$DAY.lua"
    mv dayXX_spec.lua "day${DAY}_spec.lua"
    echo "Generated src/day$DAY"
