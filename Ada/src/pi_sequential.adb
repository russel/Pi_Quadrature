with Ada.Calendar;
use Ada.Calendar;

with output;

procedure pi_sequential is
   n:Natural := 100000000;  -- 10 times fewer due to speed issues.
   d:Long_Float := 1.0 / long_float(n); -- delta is a keyword in Ada
   pi:Long_Float := 0.0;
   startTime:Time := Clock;
begin
   for i in 1..n loop
      pi := pi + 1.0 / (1.0 + ((Long_Float(i) - 0.5) * d) ** 2);
   end loop;
   pi := 4.0 * d * pi;
   output.output("Pi_Sequential", pi, n, Clock - startTime);
end pi_sequential;
