with Ada.Calendar;
use Ada.Calendar;

with Ada.Text_IO; use Ada.Text_IO;

with output;

procedure pi_tasks_accumulator is

   protected type Result_Accumulator is
      entry Initialize(n:Natural);
      entry Add(x:Long_Float);
      entry Get(x: out Long_Float);
   private
      expectedNumber:Natural := 0;
      count:Natural := 0;
      value:Long_Float := 0.0;
   end Result_Accumulator;

   protected body Result_Accumulator is
      entry Initialize(n:Natural) when True is -- expectedNumber = 0 is
      begin
         expectedNumber := n;
      end Initialize;
      entry Add(x:Long_Float) when expectedNumber > 0 and then count < expectedNumber is
      begin
         value := value + x;
         count := count + 1;
      end Add;
      entry Get(x: out Long_Float) when count = expectedNumber is
      begin
         x := value;
      end Get;
   end Result_Accumulator;

   procedure execute(numberOfTasks:Natural) is
      n:Natural := 100000000;  -- 10 times fewer due to speed issues.
      d:Long_Float := 1.0 / long_float(n); -- delta is a keyword in Ada
      pi:Long_Float := 0.0;
      startTime:Time := Clock;
      sliceSize:Natural := n / numberOfTasks;
      sum:Result_Accumulator;

      task type PartialSum (id:Natural);

      task body PartialSum is
         start:Natural;
         finish:Natural;
         localSum:Long_Float := 0.0;
      begin
         start := 1 + id * sliceSize;
         finish := (id + 1) * sliceSize;
         for i in start..finish loop
            localSum := localSum + 1.0 / (1.0 + ((Long_Float(i) - 0.5) * d) ** 2);
         end loop;
         sum.Add(localSum);
      end PartialSum;

      type RefPartialSum is access PartialSum;
      tasks:array(0..(numberOfTasks - 1)) of RefPartialSum;

   begin
      sum.Initialize(numberOfTasks);
      for i in 0..numberOfTasks - 1 loop
      	tasks(i) := new PartialSum(i);
      end loop;
      sum.Get(pi);
      pi := 4.0 * d * pi;
      output.output("Pi_Tasks_Accumulator", pi, n, Clock - startTime, numberOfTasks);
   end execute;

begin
   execute(1);
   execute(2);
   execute(8);
   execute(32);
end pi_tasks_accumulator;
