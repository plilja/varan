for (i :: Int := 0; i < 100; i := i + 1) {
    if (i < 100) {
        continue;
    }
    assert(false);
}
print("Success!");
