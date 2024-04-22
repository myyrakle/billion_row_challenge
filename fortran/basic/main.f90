subroutine solution(result)
    type Status 
        character(len=50) :: city_name
        integer(8) :: min 
        integer(8) :: max
        integer(8) :: total 
        integer(8) :: count
    end type Status

    integer :: avg

    type(Status), dimension(100) :: status_list

    ! 최종 반환값
    character(len=7000) :: result

    ! 파일 입출력을 위한 임시 변수
    character(len=300) :: line

    ! Split용 임시 변수
    character(len=30) :: measurement
    logical :: second_mode
    integer :: string_len
    integer :: pos

    ! 각 루프에서의 도시명과 측정값을 저장할 변수
    character(len=30) :: city_name
    integer :: measurement_int

    ! format용 변수
    character(len=30) :: min_str 
    character(len=30) :: max_str
    character(len=30) :: avg_str
    character(len=30) :: total_str
    character(len=30) :: count_str

    result = ""

    open(unit=10, file='measurements.txt', status='old', action='read')
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit

        city_name = ""
        measurement = ""

        ! split 처리
        second_mode = .false.

        string_len = len(line)
        do pos = 1, string_len
            if (line(pos:pos) == ';') then
                second_mode = .true.
                cycle
            end if

            if (second_mode) then
                measurement = trim(measurement) // line(pos:pos)
            else
                city_name = trim(city_name) // line(pos:pos)
            end if
        end do

        read(measurement, *) measurement_int

        ! status_list를 조회해서, 기존에 있다면 값을 업데이트하고, 없다면 새로 추가합니다.
        do i = 1, 100
            if (status_list(i)%city_name == "") then
                status_list(i)%city_name = city_name
                status_list(i)%min = measurement_int
                status_list(i)%max = measurement_int
                status_list(i)%total = measurement_int
                status_list(i)%count = 1
                exit
            end if

            if (status_list(i)%city_name == city_name) then
                status_list(i)%min = min(status_list(i)%min, measurement_int)
                status_list(i)%max = max(status_list(i)%max, measurement_int)
                status_list(i)%total = status_list(i)%total + measurement_int
                status_list(i)%count = status_list(i)%count + 1
                exit
            end if
        end do
    end do
    close(10)

    ! status_list를 city_name 기준으로 정렬합니다.
    do i = 1, 100
        if (status_list(i)%city_name == "") exit

        do j = i + 1, 100
            if (status_list(j)%city_name == "") exit

            if (status_list(i)%city_name > status_list(j)%city_name) then
                status_list(i)%city_name = status_list(i)%city_name // status_list(j)%city_name
                status_list(j)%city_name = status_list(i)%city_name // status_list(j)%city_name
                status_list(i)%city_name = status_list(i)%city_name // status_list(j)%city_name
            end if
        end do
    end do

    ! status_list를 출력형태로 변환합니다.
    do i = 1, 100
        if (status_list(i)%city_name == "") exit

        avg = status_list(i)%total / status_list(i)%count

        write (status_list(i)%min, *) min_str
        write (status_list(i)%max, *) max_str
        write (avg, *) avg_str
        write (status_list(i)%total, *) total_str
        write (status_list(i)%count, *) count_str

        result = trim(result) // '\n' // status_list(i)%city_name // '=' // &
            min_str // ';' // &
            max_str // ';' // &
            avg_str // '(' // &
            total_str // '/' // &
            count_str // ')'  
    end do
end subroutine solution

program main
    character(len=8) :: date, time, zone
    integer,dimension(8) :: values
    integer :: elapsed_time
    character(len=50) :: elapsed_time_str
    
    character(len=7000) :: result

    character(len=300) :: line
    character(len=7000) :: expect_output

    line = ""
    expect_output = ""

    ! 답안 정보를 가져옵니다.
    open(unit=10, file='outputs.txt', status='old', action='read')
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit

        if (expect_output == "") then 
            expect_output = line 
            cycle
        end if

        expect_output = trim(expect_output) // '\n' // line 
    end do
    close(10)

    ! print *, expect_output

    ! open(unit=10, file='foo.txt', status='unknown', action='write')
    ! write(10, *) expect_output
    ! close(10)

    call date_and_time(date, time, zone, values)
    print *, 'Start: ', values(5), ':', values(6), ':', values(7), '.', values(8)

    call solution(result)

    call date_and_time(date, time, zone, values)
    print *, 'Finish: ', values(5), ':', values(6), ':', values(7), '.', values(8)

    if (result == expect_output) then
        print *, 'Test Passed'
    else
        print *, 'Test Failed'
        print *, 'Expected: ' // expect_output
        print *, 'Got: ' // result
    end if
end program main
