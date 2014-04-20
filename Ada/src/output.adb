with Ada.Text_IO, Ada.Calendar;
use Ada.Text_IO, Ada.Calendar;

package body output is
   procedure output(banner:String; pi:Long_Float; n:Natural; elapseTime:Duration) is
   begin
      Put_Line("================  " & banner);
      Put_Line("    pi = " & Long_Float'Image(pi));
      Put_Line("    iteration count = " & Natural'Image(n));
      Put_Line("    elapse time = " & Duration'Image(elapseTime));
   end output;

   procedure output(banner:String; pi:Long_Float; n:Natural; elapseTime:Duration; numberOfTasks:Natural) is
   begin
      output(banner & ":  number of tasks = " & Natural'Image(numberOfTasks), pi, n, elapseTime);
   end output;
end output;
