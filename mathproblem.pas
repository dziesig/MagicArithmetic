unit MathProblem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, Generics1, TextIO1;

type
  TMathProblemType = (mpAdd, mpSub, mpMul, mpDiv);
  TMathProblemSelect = set of TMathProblemType;

  { TMathProblem }

  TMathProblem = class( TPersists )
  private
    function GetAns: String;
    function GetOk: Boolean;
    function GetOp1: String;
    function GetOp2: String;
    function GetResultStr: String;
    function GetTypeName: String;
    function GetTypeStr: String;
    function GetUserAns: String;
    procedure SetTime(AValue: Integer);
    procedure SetUserAns(AValue: String);
  protected
    fOp1, fOp2, fAns : Integer;
    fUserAns       : Integer;
    fType          : TMathProblemType;
    fTime          : Integer;
    fAbandoned     : Boolean;
  public
    constructor Create( aParent : TPersists; aSelect : TMathProblemSelect );

    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
    procedure MakeNew; override;
    procedure Abandon;

    function SameProblem( Problem : TMathProblem ): Boolean;
    function SameProblemType( Problem : TMathProblem ) : Boolean;

    property Operand1  : String read GetOp1;
    property Operand2  : String read GetOp2;
    property Answer    : String read GetAns;
    property TypeStr   : String read GetTypeStr;
    property TypeName  : String read GetTypeName;
    property UserAns   : String read GetUserAns write SetUserAns;
    property Time      : Integer read fTime write SetTime;
    property Results   : String read GetResultStr;
    property OK        : Boolean read GetOk;
    property Abandoned : Boolean read fAbandoned;
  end;

  TMathProblemListBase = specialize TPersistsList<TMathProblem>;

  TMathProblemStatistics = record
    OverallAverageScore       : Double;    // All problems all types
    OverallCount              : Integer;
    OverallAverageTime        : Double;
    OverallAbandonedCount     : Integer;
    ProblemAverageScore       : Double;    // All problems matching current problem;
    ProblemCount              : Integer;
    ProblemAverageTime        : Double;
    ProblemAbandonedCount     : Integer;
    ProblemKindAverageScore   : Double;    // All problems of current problem's type
    ProblemKindCount          : Integer;
    ProblemKindAverageTime    : Double;
    ProblemKindAbandonedCount : Integer;
  end;

  { TMathProblemList }

  TMathProblemList = class(TMathProblemListBase)
  public
     procedure Statistics( Problem : TMathProblem; var Stats : TMathProblemStatistics );
  end;

implementation

uses
  Common1, ObjectFactory1;

{ TMathProblem }

const
  Version = 1;

{ TMathProblemList }

procedure TMathProblemList.Statistics(Problem: TMathProblem;
  var Stats: TMathProblemStatistics);
var
  I : Integer;
  P : TMathProblem;
begin
  with Stats do
    begin
      // Zero the stats accumulators
      OverallAverageScore       := 0.0;
      OverallCount              := 0;
      OverallAverageTime        := 0.0;
      OverallAbandonedCount     := 0;
      ProblemAverageScore       := 0.0;    // All problems matching current problem;
      ProblemCount              := 0;
      ProblemAverageTime        := 0.0;
      ProblemAbandonedCount     := 0;
      ProblemKindAverageScore   := 0.0;    // All problems of current problem's type
      ProblemKindCount          := 0;
      ProblemKindAverageTime    := 0.0;
      ProblemKindAbandonedCount := 0;

      for I := 0 to pred(Count) do
        begin
          P := Items[I];
          if P.Abandoned then
            Inc(OverallAbandonedCount)
          else
            begin
              if P.Ok then
                OverallAverageScore := OverallAverageScore + 1.0;
              Inc(OverallCount);
              OverallAverageTime := OverallAverageTime + P.Time;
            end;
          if P.SameProblem( Problem ) then
            begin
              if P.Abandoned then
                Inc(ProblemAbandonedCount)
              else
                begin
                  if P.Ok then
                    ProblemAverageScore := ProblemAverageScore + 1.0;
                  Inc(ProblemCount);
                  ProblemAverageTime := ProblemAveragetime + P.Time;
                end;
            end;
          if P.SameProblemType( Problem ) then
            begin
              if P.Abandoned then
                Inc(ProblemKindAbandonedCount)
              else
                begin
                  if P.Ok then
                    ProblemKindAverageScore := ProblemKindAverageScore + 1.0;
                  Inc(ProblemKindCount);
                  ProblemKindAverageTime := ProblemKindAveragetime + P.Time;
                end;
            end;
        end;
      OverallAverageScore := OverallAverageScore / Max( 1.0, OverallCount );
      OverallAverageTime  := OverallAveragetime  / Max( 1.0, OverallCount );
      ProblemAverageScore := ProblemAverageScore / Max( 1.0, ProblemCount );
      ProblemAverageTime  := ProblemAverageTime  / Max( 1.0, ProblemCount );
      ProblemKindAverageScore := ProblemKindAverageScore / Max( 1.0, ProblemKindCount );
      ProblemKindAverageTime  := ProblemKindAverageTime  / Max( 1.0, ProblemKindCount );
    end;

end;

procedure TMathProblem.Abandon;
begin
  fAbandoned := True;
end;

constructor TMathProblem.Create(aParent: TPersists; aSelect: TMathProblemSelect
  );
var
  Done      : Boolean;
begin
  Create( aParent );
  if aSelect = [] then
    raise Exception.create('MathProblem created with empty selection set.');
  Done := False;
  repeat
    fType := TMathProblemType(Random( ord(mpDiv) + 1 ));
    Done := fType in aSelect;
    if Done then
      begin
        case fType of
          mpAdd:
            begin
              fOp1 := Random(201) - 100;
              fOp2 := Random(201) - 100;
              fAns := fOp1 + fOp2;
            end;
          mpSub:
            begin
              fOp1 := Random(201) - 100;
              fOp2 := Random(201) - 100;
              fAns := fOp1 - fOp2;
            end;
          mpMul:
            begin
              fOp1 := Random(25) - 12;
              fOp2 := Random(25) - 12;
              fAns := fOp1 * fOp2;
            end;
          mpDiv:
            begin
              fAns := Random(25) - 12;
              fOp2 := Random(25) - 12;
              if fOp2 = 0 then fOp2 := 1;// no div by zero
              fOp1 := fAns * fOp2;
            end;
        end;
      end;
  until Done;
end;

function TMathProblem.GetAns: String;
begin
  Result := IntToStr( fAns );
end;

function TMathProblem.GetOk: Boolean;
begin
  Result := fUserAns = fAns;
end;

function TMathProblem.GetOp1: String;
begin
  Result := IntToStr( fOp1 );
end;

function TMathProblem.GetOp2: String;
begin
  Result := IntToStr( fOp2 );
end;

function TMathProblem.GetResultStr: String;
begin
  if OK then
    Result := 'Correct!!!!!'
  else
    Result := 'Sorry, the correct answer is ' + IntToStr( fAns );
end;

function TMathProblem.GetTypeName: String;
const
  TS : array [TMathProblemType] of String = ('Addition','Subtraction','Multiplication','Division');
begin
  Result := TS[fType];
end;

function TMathProblem.GetTypeStr: String;
const
  TS : array [TMathProblemType] of String = ('+','-','x','/');
begin
  Result := TS[fType];
end;

function TMathProblem.GetUserAns: String;
begin
  Result := IntToStr( fUserAns );
end;

procedure TMathProblem.MakeNew;
begin
  inherited MakeNew;
  fOp1 := 1;
  fOp2 := 1;
  fAns := 2;
  fUserAns := 0;
  fType := mpAdd;
  fTime := 0;
  fAbandoned := false;
end;

procedure TMathProblem.Read(TextIO: TTextIO; Version: Integer);
var
  Temp : Integer;
begin
  MakeNew;
  if Version >= 1 then
    begin
      TextIO.ReadLn( Temp );
      fType := TMathProblemType( Temp );
      TextIO.ReadLn( fOp1 );
      TextIO.ReadLn( fOp2 );
      TextIO.ReadLn( fAns );
      TextIO.ReadLn( fUserAns );
      TextIO.ReadLn( fTime );
      TextIO.ReadLn( fAbandoned );
    end;
end;

function TMathProblem.SameProblem(Problem: TMathProblem): Boolean;
begin
  Result := SameProblemType( Problem );
  if Result then
    Result := (Problem.fOp1 = fOp1) and (Problem.fOp2 = fOp2);
end;

function TMathProblem.SameProblemType(Problem: TMathProblem): Boolean;
begin
  Result := Problem.fType = fType;
end;

procedure TMathProblem.Save(TextIO: TTextIO);
begin
  SaveHeader( TextIO, Version );
  TextIO.Writeln( ord(fType) );
  TextIO.Writeln( fOp1 );
  TextIO.Writeln( fOp2 );
  TextIO.Writeln( fAns );
  TextIO.Writeln( fUserAns );
  TextIO.Writeln( fTime );
  TextIO.WriteLn( fAbandoned );
  SaveTrailer( TextIO );
end;

procedure TMathProblem.SetTime(AValue: Integer);
begin
  if fTime=AValue then Exit;
  fTime:=AValue;
end;

procedure TMathProblem.SetUserAns(AValue: String);
begin
  fUserAns := StrToInt(AValue);
end;

initialization
  ObjectFactory.RegisterClass( TMathProblem.ClassType );
  ObjectFactory.RegisterClass( TMathProblemList.ClassType );

end.

