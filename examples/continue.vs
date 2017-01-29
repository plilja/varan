print("This program shouldn't print any output (except this line)");
for (i :: Int := 0; i < 100; i := i + 1) {
    if (i < 100) {
        continue;
    }
    print("Continue statement doesn't work");
}
