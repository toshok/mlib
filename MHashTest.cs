using System;
using System.Runtime.InteropServices;
using MHash;

struct DependencyProperty {
	public IntPtr native;
	public string name;
}

unsafe class MHashTest {
	static uint hashfunc (IntPtr key) { return (uint)key; }
	static bool equalfunc (IntPtr a, IntPtr b) { return a == b; }


	[StructLayout (LayoutKind.Explicit)]
	unsafe struct Map {
		[FieldOffset(0)] public IntPtr intptr;
		[FieldOffset(0)] public object obj;
	}

	public void RunTest ()
	{
		MHashTable *hash = create_hashtable();
		Map map;

		DependencyProperty dp1, dp2;

		dp1 = new DependencyProperty ();
		dp1.native = (IntPtr)0x01;
		dp1.name = "DP-0x01";

		map.obj = dp1;
		hash->Insert (dp1.native, map.intptr);

		dp2 = new DependencyProperty ();
		dp2.native = (IntPtr)0x02;
		dp2.name = "DP-0x02";

		map.obj = dp2;
		hash->Insert (dp2.native, map.intptr);

		IntPtr ip = hash->Lookup ((IntPtr)0x01);

		if (ip == IntPtr.Zero) {
			Console.WriteLine ("null!?");
		}
		else {
			map.intptr = ip;
			DependencyProperty prop = (DependencyProperty)map.obj;

			Console.WriteLine ("dp property = {0}", prop.name);
		}

		test_hashtable ((IntPtr)hash);
	}

	public static void Main (string[] args) {
		MHashTest test = new MHashTest ();

		test.RunTest ();
	}

	[DllImport ("mhashtest")]
	public static extern void test_hashtable (IntPtr table);

	[DllImport ("mhashtest")]
	public static extern MHashTable* create_hashtable ();
}