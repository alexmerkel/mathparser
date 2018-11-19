program Sample;

{$B-}

uses
  Calculator, Classes, Dialogs;

var
  Calc: TCalculator;
  List: TStrings;
begin
  Calc := TCalculator.Create(nil);
  try
    List := TStringList.Create;
    try
      List.Add('A = 1 + 2');
      List.Add('B = 3 + 4');
      List.Add('C = A + B');
      Calc.AddVariableList(List);
    finally
      List.Free;
    end;
    ShowMessage(Calc.AsString('A + B + C'));
  finally
    Calc.Free;
  end;
end.
