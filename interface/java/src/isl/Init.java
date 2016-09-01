package isl;

public class Init {

  private static boolean loaded = false;

  public static void loadNative() {
    loadNative(null);
  }

  public static void loadNative(String libPath) {
    if (loaded)
      return;

    String libName = "isl_jni";
    if (libPath == null)
      System.loadLibrary(libName);
    else
      System.load(libPath + "/" + System.mapLibraryName(libName));
    loaded = true;
  }
}
