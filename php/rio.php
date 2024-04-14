<?php

require_once __DIR__ . '/common.php';

function challenge()
{
    $file = fopen(MEASUREMENTS_PATH, 'r');

    $arr = [];
    while (($line = stream_get_line($file, 99, "\n")) !== false) {
        $temp = explode(';', $line);
        $key = $temp[0];
        $value = (int)$temp[1];
        $current_arr = &$arr[$key];
        if ($current_arr !== null) {
            if ($current_arr[0] < $value) {
                $current_arr[0] = $value;
            }

            if ($current_arr[1] > $value) {
                $current_arr[1] = $value;
            }

            ++$current_arr[2];
            $current_arr[3] += $value;
        } else {
            $arr[$key] = [
                0 => $value,
                1 => $value,
                2 => 1,
                3 => 1,
            ];
        }
    }
    fclose($file);
    ksort($arr);

    $output = '';
    foreach ($arr as $key => $value) {
        $avg = (int)($value[3] / $value[2]);
        $output .= "$key={$value[1]};{$value[0]};$avg($value[3]/$value[2])" . PHP_EOL;
    }

    return $output;
}

if(ini_get('opcache.enable_cli') != '1'){
    echo 'Performance issue please set php.ini opcache.enable_cli=1' . PHP_EOL;
}

$start = microtime(true);
$output = challenge();
$file = fopen(OUT_PUT_PATH, 'w');
fputs($file, $output);
$end = microtime(true);

echo $end - $start;