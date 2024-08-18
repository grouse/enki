# TODO
- [ ] meta/test: sub-categories with auto relationship deduction
    - e.g. CATEGORY("maths/vec3/misc"), CATEGORY("containers/array")
- [ ] move generated header folders into target src dirs
    - each target needs to be able to just #include "gen/file" and not have things polluted by outside targets
    - I want to be able to commit the generated headers, allowing better change tracking and distribution
