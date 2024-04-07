import sys, time
from common import OUTPUT_PATH, MEASUREMENTS_PATH, Timer

def solution(path: str) -> str:
    map = {}
    with open(path, 'r') as f:
        for line in f:
            city_name, measurement = line.strip().split(";")
            measurement = int(measurement)
            
            if map.get(city_name, None) is not None:
                map[city_name] = dict(
                min = min(map[city_name]["min"], measurement),
                max = max(map[city_name]["max"], measurement),
                total = map[city_name]["total"] + measurement,
                count = map[city_name]["count"] + 1,
                )
            
            else:
                map[city_name] = dict(
                    min= measurement,
                    max= measurement,
                    total= measurement,
                    count= 1,
                )
                
    sorted_map = dict(sorted(map.items(), key=lambda x: x[0]))
    bucket = ""
    for city_name, status in sorted_map.items():
        avg = int(status["total"] / status["count"])
        line = "{}={};{};{}({}/{})\n".format(city_name, status["min"], 
                      status["max"], avg, status["total"], status["count"])
        bucket += line
    return bucket

def main():
    with open(OUTPUT_PATH, 'r') as f:
        expect_output = f.read()
    timer = Timer()
    got = solution(MEASUREMENTS_PATH)
    print(f"Elapsed: {timer.elapsed_as_milliseconds()}ms")
    
    assert expect_output == got
    
if __name__ == "__main__":
    main()