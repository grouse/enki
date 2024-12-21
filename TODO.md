# TODO
- [ ] resolve subcategories into parent/child hierarchy


# DONE
- [x] meta depfile support
    - e.g. for meta(tests.cpp) to auto-regenerate based on all the #includes its doing
- [x] move generated header folders into target src dirs
    - each target needs to be able to just #include "gen/file" and not have things polluted by outside targets
    - I want to be able to commit the generated headers, allowing better change tracking and distribution
