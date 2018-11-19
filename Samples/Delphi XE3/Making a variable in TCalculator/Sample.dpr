program Sample;

{$B-}

// The sample demostrates how TCalculator variables are easy to use

uses
  SysUtils, Dialogs, Calculator;

var
  Calc: TCalculator;
begin
  Calc := TCalculator.Create(nil);
  try
// ------------------------------------------------------------------------- (1)
    // Here we declare a variable "A" and set its value to 10:

    Calc.ItemValue['A'] := '10';

    // Here we declare one more variable "B" and set its value to "A":

    Calc.ItemValue['B'] := 'A';

    // Now we can use both variables in mathematic expression:

    ShowMessage ( Calc.AsString ( 'A + B' ) );
// ------------------------------------------------------------------------- (2)
    // In the sample above "B" does not depend on "A", inspite of it was
    // initialized to "A". Hence "B" remains unchanged when we change "A":

    Calc.ItemValue['A'] := '20';

    // "B" still equals 10:
    ShowMessage ( Calc.AsString ( 'B' ) );
// ------------------------------------------------------------------------- (3)
    // But it is possible making a dependency between two or more variables.
    // To do that we first need mark "A" as a not optimizable variable:

    Calc.Optimizable['A'] := False;

    // Now all variables which where initialized to an expression where "A"
    // takes part, become dependend on "A":

    Calc.ItemValue['C'] := 'A + 1';
    Calc.ItemValue['D'] := 'A + 2';
// ------------------------------------------------------------------------- (4)
    // Both variables, "C" and "D" are dependent on "A" and change its value
    // each time when "A" changes:

    Calc.ItemValue['A'] := '30';

    // "C" now equals 31 and "D" now equals 32:
    ShowMessage ( Calc.AsString ( 'C' ) );
    ShowMessage ( Calc.AsString ( 'D' ) );
// ------------------------------------------------------------------------- (5)
    // It is important to understand that setting up the variable value to the
    // expression where the same variable presents causes an exception:

    try
      // Try to invert "A" value:
      Calc.ItemValue['A'] := '- A';
    except
      // We have an exception: "Function "A" recursively uses itself"
      on E: Exception do ShowMessage(E.Message);
    end;

    // And this is logically corrent. If variable is marked as
    // not optimizable then it cannot refer to itself.
// ------------------------------------------------------------------------- (6)
    // In case when we try to update a variable and an exception is raised,
    // TCalculator sets variable to TCalculator.ErrorValue property to prevent
    // the following exceptions.
    // Here we restore the lost "A" value:

    Calc.ItemValue['A'] := '2 + 2';
// ------------------------------------------------------------------------- (7)
    // And make sure that variables "C" and "D" change its value due to "A"
    // variable is also changed:
    ShowMessage ( Calc.AsString ( 'C' ) );
    ShowMessage ( Calc.AsString ( 'D' ) );

// ------------------------------------------------------------------------- (9)
    // Please note that you may build a dependency chain of any deep:

    // Clear all (cache and previously created items in particular):
    Calc.Clear;

    Calc.ItemValue['A'] := '2 + 2';
    Calc.Optimizable['A'] := False;

    // Please take into account that not optimizable variable can be created
    // by adding an exclamation sign before its name. It means that previous
    // two code-lines are equivalent to the following code:
    // Calc.ItemValue['!A'] := '2 + 2';

    // Due to the first element (variable "A") is not optimizable, all
    // subsequent elements are not optimizable too:

    Calc.ItemValue['B'] := 'A + 2';
    Calc.ItemValue['C'] := 'B * 2';
    Calc.ItemValue['D'] := 'C - 2';
    Calc.ItemValue['E'] := 'D / 2';

    ShowMessage ( Calc.AsString ( 'E' ) );

    // The last element of the chain changes its value if any previous element
    // is changed as well:
    Calc.ItemValue['A'] := '3 + 3';

    ShowMessage ( Calc.AsString ( 'E' ) );

// ------------------------------------------------------------------------ (10)
    // The chain becomes broken when some element looses its dependency from the
    // previous one:

    Calc.ItemValue['C'] := Calc.AsString( 'C' );

    // Here we change first element of the chain:

    Calc.ItemValue['A'] := '4 + 4';

    // The last element of the chain is not changed:
    ShowMessage ( Calc.AsString ( 'E' ) );
  finally
    Calc.Free;
  end;
end.
