from common import OUTPUT_PATH, MEASUREMENTS_PATH, Timer
import polars

def solution(path: str) -> str:
    dataframe = polars.scan_csv(path, separator=";", new_columns=['name', 'value'])
    result = dataframe.group_by('name').agg(
        polars.col('value').min().alias('min'),
        polars.col('value').max().alias('max'),
        polars.col('value').sum().alias('sum'),
        polars.col('value').count().alias('count')
    ).sort('name').collect()

    bucket = ""
    for row in result.rows(named=True):
        city_name = row['name']
        status = dict(
            min = row['min'],
            max = row['max'],
            total = row['sum'],
            count = row['count'],
        )

        avg = status["total"] // status["count"]
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
    
    if expect_output == got:
        print("PASS")
    else:
        print("FAIL")
        print(f"Expect: {expect_output}")
        print(f"Got: {got}")
    
if __name__ == "__main__":
    main()