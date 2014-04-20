with Ada.Calendar;
use Ada.Calendar;

package output is
   procedure output(banner:string; pi:long_float; n:natural; elapseTime:Duration);
   procedure output(banner:string; pi:long_float; n:natural; elapseTime:Duration; numberOfTasks:Natural);
end output;
