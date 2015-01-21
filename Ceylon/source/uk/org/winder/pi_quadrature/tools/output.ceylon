import java.lang { Runtime { runtime } }

"A function to output stuff."
by("Russel Winder")
shared void output(String name, Float pi, Integer n, Float elapseTime) {
	print("========================== ``name``");
	print("\tπ = ``pi``");
	print("\titeration count = ``n``");
	print("\telapse time = ``elapseTime``");
}

shared void outputN(String name, Float pi, Integer n, Float elapseTime, Integer numberOfTasks) {
	output(name + ": number of tasks ``numberOfTasks``", pi, n, elapseTime);
	print("\tnumber of processors = ``runtime.availableProcessors()``");
}
