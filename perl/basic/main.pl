#!/usr/bin/env perl
# Perl Basic Implementation for Billion Row Challenge

use strict;
use warnings;
use Time::HiRes qw(time);

# 파일 경로 상수
my $OUTPUT_PATH = "outputs.txt";
my $MEASUREMENTS_PATH = "measurements.txt";

sub solution {
    my ($path) = @_;
    my %status_map;

    # 파일을 한 줄씩 읽기
    open(my $fh, '<', $path) or die "Could not open file '$path': $!";

    while (my $line = <$fh>) {
        chomp $line;

        # 세미콜론으로 분리
        my @parts = split /;/, $line, 2;
        next unless @parts == 2;

        my $city_name = $parts[0];
        my $measurement = $parts[1];

        # 숫자 검증
        next unless $measurement =~ /^-?\d+$/;
        $measurement = int($measurement);

        # 통계 업데이트
        if (exists $status_map{$city_name}) {
            my $status = $status_map{$city_name};
            $status->{min} = $measurement if $measurement < $status->{min};
            $status->{max} = $measurement if $measurement > $status->{max};
            $status->{total} += $measurement;
            $status->{count}++;
        } else {
            # 새 도시 추가
            $status_map{$city_name} = {
                min => $measurement,
                max => $measurement,
                total => $measurement,
                count => 1
            };
        }
    }

    close $fh;

    # 도시 이름으로 정렬
    my @sorted_cities = sort keys %status_map;

    # 결과 문자열 생성
    my $result = "";
    foreach my $city_name (@sorted_cities) {
        my $status = $status_map{$city_name};
        my $avg = int($status->{total} / $status->{count});
        $result .= sprintf("%s=%d;%d;%d(%d/%d)\n",
            $city_name,
            $status->{min},
            $status->{max},
            $avg,
            $status->{total},
            $status->{count});
    }

    return $result;
}

sub read_file {
    my ($path) = @_;
    open(my $fh, '<', $path) or die "Could not open file '$path': $!";
    local $/ = undef;
    my $content = <$fh>;
    close $fh;
    return $content;
}

sub main {
    # 예상 출력 읽기
    my $expected_output = read_file($OUTPUT_PATH);

    # 타이머 시작
    my $start_time = time();
    my $result = solution($MEASUREMENTS_PATH);
    my $elapsed = time() - $start_time;
    my $elapsed_ms = int($elapsed * 1000);

    print "Elapsed: ${elapsed_ms}ms\n";

    # 결과 비교
    if ($expected_output eq $result) {
        print "Matched!\n";
    } else {
        print "Output does not match expected!\n";

        # 디버깅용 첫 몇 줄 출력
        my @expected_lines = split /\n/, $expected_output;
        my @result_lines = split /\n/, $result;

        print "Expected first 3 lines:\n";
        for (my $i = 0; $i < 3 && $i < scalar @expected_lines; $i++) {
            print "  $expected_lines[$i]\n";
        }

        print "Got first 3 lines:\n";
        for (my $i = 0; $i < 3 && $i < scalar @result_lines; $i++) {
            print "  $result_lines[$i]\n";
        }
    }
}

# 메인 실행
main();
