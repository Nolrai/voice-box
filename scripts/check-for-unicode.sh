# short script to check for non-ASCII characters in Haskell source files
# exits with code 1 if any non-ASCII characters are found

if rg -q '[^\x00-\x7F]' src-lib/ src-exe/ src-test/; then
    echo "Found non-ASCII characters"
    rg -n '[^\x00-\x7F]' src-lib/ src-exe/ src-test/
    exit 1
else
    echo "All ASCII ✓"
    exit 0
fi