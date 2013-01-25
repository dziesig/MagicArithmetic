//Copyright (c) 2013 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of the MagicArithmetic program.
//
//MagicArithmetic is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicArithmetic is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicDispatcher.  If not, see <http://www.gnu.org/licenses/>.

unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus,

  MathProblem;

type

  { TForm1 }

  TForm1 = class(TForm)
    AbandonedCount1: TLabel;
    AbandonedCount2: TLabel;
    Button1: TButton;
    CheckGroup1: TCheckGroup;
    CompletedCount1: TLabel;
    CompletedCount2: TLabel;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    CompletedCount: TLabel;
    AbandonedCount: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    Score: TLabel;
    Score1: TLabel;
    Score2: TLabel;
    Seconds: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Negative: TLabel;
    Seconds1: TLabel;
    Seconds2: TLabel;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }

    fActive : Boolean;

    Problem  : TMathProblem;
    ProblemList : TMathProblemList;
    procedure CreateProblem;
    procedure CheckAnswer;
    function  AtLeastOneProblemTypeSelected : Boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Stringsubs, Common1, TextIO1;

{ TForm1 }

function TForm1.AtLeastOneProblemTypeSelected: Boolean;
var
  Kind : TMathProblemType;
begin
  Result := False;
  for Kind := mpAdd to mpDiv do
    if CheckGroup1.Checked[ord(Kind)] then
      begin
        Result := True;
        exit;
      end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Visible := True;
  Label2.Visible := True;
  Label3.Visible := True;
  Label4.Visible := True;
  Label5.Visible := False;
  Label6.Visible := True;
  Edit1.Visible  := True;
  Button1.Visible := False;
  Negative.Visible := False;
  CreateProblem;
  Timer1.Enabled := True;
  fActive := True;
end;

procedure TForm1.CheckAnswer;
var
  UserAnswer : Integer;
  Stats      : TMathProblemStatistics;
begin
  Timer1.Enabled := False;
  fActive := False;
  if Negative.Visible then
    Problem.UserAns := '-' + Edit1.Text
  else
    Problem.UserAns := Edit1.Text;
  Label5.Caption := Problem.Results;
  Label5.Visible := True;
  Button1.Visible := True;
  ProblemList.Statistics( Problem, Stats );
  CompletedCount.Caption := IntToStr( Stats.OverallCount );
  AbandonedCount.Caption := IntToStr( Stats.OverallAbandonedCount );
  Score.Caption          := FloatToStrF( Stats.OverallAverageScore*100.0, ffFixed, 7,2 ) + '%';
  Seconds.Caption        := FloatToStrF( Stats.OverallAverageTime, ffFixed, 7,1 ) + ' Seconds';

  GroupBox2.Caption := ' ' + Problem.Operand1+ ' ' + Problem.TypeStr + ' ' + Problem.Operand2 + ' ';
  CompletedCount1.Caption := IntToStr( Stats.ProblemCount );
  AbandonedCount1.Caption := IntToStr( Stats.ProblemAbandonedCount );
  Score1.Caption          := FloatToStrF( Stats.ProblemAverageScore*100.0, ffFixed, 7,2 ) + '%';
  Seconds1.Caption        := FloatToStrF( Stats.ProblemAverageTime, ffFixed, 7,1 ) + ' Seconds';

  GroupBox3.Caption := Problem.TypeName;
  CompletedCount2.Caption := IntToStr( Stats.ProblemKindCount );
  AbandonedCount2.Caption := IntToStr( Stats.ProblemKindAbandonedCount );
  Score2.Caption          := FloatToStrF( Stats.ProblemKindAverageScore*100.0, ffFixed, 7,2 ) + '%';
  Seconds2.Caption        := FloatToStrF( Stats.ProblemKindAverageTime, ffFixed, 7,1 ) + ' Seconds';

  GroupBox1.Visible := True;
  GroupBox2.Visible := True;
  GroupBox3.Visible := True;
end;

procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
  Button1.Visible := AtLeastOneProblemTypeSelected;
  Label19.visible := not Button1.Visible;
end;

procedure TForm1.CreateProblem;
var
  Kind : TMathProblemType;
  Select : TMathProblemSelect;
  I : Integer;
begin
  Select := [];
  for Kind := mpAdd to mpDiv do
    begin
      if CheckGroup1.Checked[ord(Kind)] then
        Select := Select + [Kind];
    end;
  Problem := TMathProblem.Create( ProblemList, Select );
  ProblemList.Add( Problem );
  Label1.Caption := Problem.Operand1;
  Label2.Caption := Problem.TypeStr;
  Label3.Caption := Problem.Operand2;
  Edit1.Text := '';
  Edit1.SetFocus;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9']) then
    if Key <> #8 then
      if Key = #13 then
        if Button1.Visible then
          Button1Click( Sender )
        else
          CheckAnswer
      else
        Key := #0;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  TextIO : TTextIO;
begin
  TextIO := TTextIO.Create( DefaultSaveLocation + 'MathProblem.dat', True );
  try
    ProblemList.Save( TextIO );
  finally
    TextIO.Free;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Ans : Integer;
begin
  Ans := mrYes;
  if fActive then
    begin
      Ans := AreYouSure( 'Close and abandon problem?' );
      if Ans = mrYes then
        Problem.Abandon;
    end;
  CanClose := Ans = mrYes;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FileName : String;
  TextIO   : TTextIO;
begin
  Randomize;
  FileName := DefaultSaveLocation + 'MathProblem.dat';
  if FileExists(FileName) then
    begin
      TextIO := TTextIO.Create( FileName, False );
      try
        ProblemList := TMathProblemList.Load( TextIO ) as TMathProblemList;
      finally
        TextIO.Free;
      end
    end
  else
    ProblemList := TMathProblemList.Create( nil );
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SpeedButton11Click(Sender: TObject);
begin
  Negative.Visible := not Negative.Visible;
end;

procedure TForm1.SpeedButton12Click(Sender: TObject);
begin
  Edit1.Text := '';
end;

procedure TForm1.SpeedButton13Click(Sender: TObject);
begin
  if Button1.Visible then
    Button1Click( Sender )
  else
    if not Empty( Edit1.Text ) then
      CheckAnswer;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  with Sender as TSpeedButton do
    Edit1.Text := Edit1.Text + IntToStr( Tag );
end;

procedure TForm1.Timer1StartTimer(Sender: TObject);
begin
  Problem.Time := 0;
  Label6.Caption := IntToStr( Problem.Time ) + ' seconds';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Problem.Time := Problem.Time + 1;
  Label6.Caption := IntToStr( Problem.Time ) + ' seconds';
end;

end.

