<?php

require_once __DIR__ . '/common.php';

function solution()
{
    $file = fopen(MEASUREMENTS_PATH, 'r');

    $arr = [];
    while (($line = stream_get_line($file, 99, "\n")) !== false) {
        $temp = explode(';', $line);
        $key = $temp[0];
        $value = (int)$temp[1];
        $current_arr = &$arr[$key];
        
        if ($current_arr !== null) {
            if ($current_arr['max'] < $value) {
                $current_arr['max'] = $value;
            }
            if ($current_arr['min'] > $value) {
                $current_arr['min'] = $value;
            }
            ++$current_arr['cnt'];
            $current_arr['total'] += $value;
        } else {
            $arr[$key] = [
                'max' => $value,
                'min' => $value,
                'cnt' => 1,
                'total' => $value,
            ];
        }
    }

    fclose($file);
    ksort($arr);

    $result = '';
    foreach ($arr as $key => $value) {
        $avg = (int)($value['total'] / $value['cnt']);
        $result .= "$key={$value['min']};{$value['max']};$avg({$value['total']}/{$value['cnt']})\n";
    }

    return $result;
}

$start = microtime(true);
$got = solution();
$end = microtime(true);
$total_time = floor(($end - $start) * 1000);

echo "Elapsed {$total_time}ms\n";

// 검증
$expect_output = file_get_contents(OUTPUT_PATH);
if ($got === $expect_output) {
    echo 'Test passed';
} else {
    echo "Test failed\n";
    echo 'Expected:';
    echo $expect_output;
    echo PHP_EOL;
    echo 'Actual:';
    echo $got;
}

