import pandas as pd
from common import OUTPUT_PATH, MEASUREMENTS_PATH, Timer

def solution(path: str) -> str:
    return '\n'.join([f"{i[1].name}={int(i[1]['value']['min'])};{int(i[1]['value']['max'])};{int(i[1]['value']['mean'])}({int(i[1]['value']['sum'])}/{int(i[1]['value']['count'])})" for i in pd.read_table(path, header=None, delimiter=";", names=['city', 'value']).groupby(['city']).agg({'value':['mean', 'max', 'min', 'count', 'sum']}).iterrows()])+"\n"

def main():
    with open(OUTPUT_PATH, 'r') as f:
        expect_output = f.read()
    timer = Timer()
    got = solution(MEASUREMENTS_PATH)
    print(f"Elapsed: {timer.elapsed_as_milliseconds()}ms")
    
    assert expect_output == got
    
if __name__ == "__main__":
    main()