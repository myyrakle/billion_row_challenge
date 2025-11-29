program BillionRowChallenge;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fgl, Math;

type
  // 통계 데이터 구조
  TStatus = record
    Min: Int64;
    Max: Int64;
    Total: Int64;
    Count: Int64;
  end;

  // HashMap을 위한 타입 정의
  TStatusMap = specialize TFPGMap<String, TStatus>;

const
  OUTPUT_PATH = 'outputs.txt';
  MEASUREMENTS_PATH = 'measurements.txt';

// 문자열을 구분자로 분리
procedure SplitString(const Str: String; Delimiter: Char; var Parts: array of String; var Count: Integer);
var
  I, Start: Integer;
begin
  Count := 0;
  Start := 1;

  for I := 1 to Length(Str) do
  begin
    if (Str[I] = Delimiter) or (I = Length(Str)) then
    begin
      if Count < Length(Parts) then
      begin
        if I = Length(Str) then
          Parts[Count] := Copy(Str, Start, I - Start + 1)
        else
          Parts[Count] := Copy(Str, Start, I - Start);
        Inc(Count);
      end;
      Start := I + 1;
      if Count >= Length(Parts) then
        Break;
    end;
  end;
end;

// 솔루션 함수
function Solution(const Path: String): String;
var
  InputFile: TextFile;
  Line: String;
  Parts: array[0..1] of String;
  PartCount: Integer;
  CityName: String;
  Measurement: Int64;
  StatusMap: TStatusMap;
  Status: TStatus;
  Index: Integer;
  SortedCities: TStringList;
  I: Integer;
  Avg: Int64;
  OutputStr: String;
begin
  StatusMap := TStatusMap.Create;
  StatusMap.Sorted := True;

  try
    // 파일을 한 줄씩 읽기
    AssignFile(InputFile, Path);
    Reset(InputFile);

    try
      while not Eof(InputFile) do
      begin
        ReadLn(InputFile, Line);

        // 세미콜론으로 분리
        SplitString(Line, ';', Parts, PartCount);

        if PartCount = 2 then
        begin
          CityName := Parts[0];

          // 숫자로 변환
          if TryStrToInt64(Parts[1], Measurement) then
          begin
            // HashMap에서 도시 찾기
            Index := StatusMap.IndexOf(CityName);

            if Index >= 0 then
            begin
              // 기존 통계 업데이트
              Status := StatusMap.Data[Index];
              if Measurement < Status.Min then
                Status.Min := Measurement;
              if Measurement > Status.Max then
                Status.Max := Measurement;
              Status.Total := Status.Total + Measurement;
              Status.Count := Status.Count + 1;
              StatusMap.Data[Index] := Status;
            end
            else
            begin
              // 새 도시 추가
              Status.Min := Measurement;
              Status.Max := Measurement;
              Status.Total := Measurement;
              Status.Count := 1;
              StatusMap.Add(CityName, Status);
            end;
          end;
        end;
      end;
    finally
      CloseFile(InputFile);
    end;

    // 도시 이름으로 정렬 (이미 Sorted := True로 정렬됨)
    SortedCities := TStringList.Create;
    try
      for I := 0 to StatusMap.Count - 1 do
        SortedCities.Add(StatusMap.Keys[I]);

      SortedCities.Sort;

      // 결과 문자열 생성
      OutputStr := '';
      for I := 0 to SortedCities.Count - 1 do
      begin
        CityName := SortedCities[I];
        Index := StatusMap.IndexOf(CityName);
        Status := StatusMap.Data[Index];
        Avg := Status.Total div Status.Count;

        OutputStr := OutputStr + Format('%s=%d;%d;%d(%d/%d)',
          [CityName, Status.Min, Status.Max, Avg, Status.Total, Status.Count]) + LineEnding;
      end;
    finally
      SortedCities.Free;
    end;
  finally
    StatusMap.Free;
  end;

  Solution := OutputStr;
end;

// 파일 전체 읽기
function ReadFile(const Path: String): String;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(Path, fmOpenRead);
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    ReadFile := StringStream.DataString;
  finally
    StringStream.Free;
    FileStream.Free;
  end;
end;

// 두 정수 중 작은 값 반환
function MinInt(A, B: Integer): Integer;
begin
  if A < B then
    MinInt := A
  else
    MinInt := B;
end;

// 메인 프로시저
procedure Main;
var
  ExpectedOutput: String;
  ResultOutput: String;
  StartTime, EndTime: TDateTime;
  ElapsedMs: Int64;
  ExpectedLines, ResultLines: TStringList;
  I: Integer;
begin
  // 예상 출력 읽기
  ExpectedOutput := ReadFile(OUTPUT_PATH);

  // 타이머 시작
  StartTime := Now;
  ResultOutput := Solution(MEASUREMENTS_PATH);
  EndTime := Now;

  ElapsedMs := Round((EndTime - StartTime) * 24 * 60 * 60 * 1000);

  WriteLn('Elapsed: ', ElapsedMs, 'ms');

  // 결과 비교
  if ExpectedOutput = ResultOutput then
  begin
    WriteLn('Matched!');
  end
  else
  begin
    WriteLn('Output does not match expected!');

    // 디버깅용 첫 몇 줄 출력
    ExpectedLines := TStringList.Create;
    ResultLines := TStringList.Create;
    try
      ExpectedLines.Text := ExpectedOutput;
      ResultLines.Text := ResultOutput;

      WriteLn('Expected first 3 lines:');
      for I := 0 to MinInt(2, ExpectedLines.Count - 1) do
        WriteLn('  ', ExpectedLines[I]);

      WriteLn('Got first 3 lines:');
      for I := 0 to MinInt(2, ResultLines.Count - 1) do
        WriteLn('  ', ResultLines[I]);
    finally
      ResultLines.Free;
      ExpectedLines.Free;
    end;
  end;
end;

// 프로그램 시작
begin
  Main;
end.
