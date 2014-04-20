with Ada.Calendar;
use Ada.Calendar;

with output;

procedure pi_tasks_reduce is

      procedure execute(numberOfTasks:Natural) is
      n:Natural := 100000000;  -- 10 times fewer due to speed issues.
      d:Long_Float := 1.0 / long_float(n); -- delta is a keyword in Ada
      pi:Long_Float := 0.0;
      startTime:Time := Clock;
      sliceSize:Natural := n / numberOfTasks;
      sum:Long_Float := 0.0;

      task type PartialSum is
         entry Start(id:Natural);
         entry Get(x:out Long_Float);
       end PartialSum;

      task body PartialSum is
         strt:Natural;
         finish:Natural;
         sum:Long_Float := 0.0;
      begin
         accept Start(id:Natural) do
            strt := 1 + id * sliceSize;
            finish := (id + 1) * sliceSize;
         end Start;
         for i in strt..finish loop
            sum := sum + 1.0 / (1.0 + ((Long_Float(i) - 0.5) * d) ** 2);
         end loop;
         accept Get(x: out Long_Float) do
            x := sum;
         end Get;
      end PartialSum;

      type RefPartialSum is access PartialSum;
      tasks:array(0..numberOfTasks - 1) of RefPartialSum;

   begin
      for i in 0..numberOfTasks - 1 loop
         tasks(i) := new PartialSum;
         tasks(i).Start(i);
      end loop;
      for i in 0..numberOfTasks - 1 loop
         tasks(i).Get(sum);
         pi := pi + sum;
      end loop;
      pi := 4.0 * d * pi;
      output.output("Pi_Tasks_Reduce", pi, n, Clock - startTime, numberOfTasks);
   end execute;

begin
   execute(1);
   execute(2);
   execute(8);
   execute(32);
end pi_tasks_reduce;
