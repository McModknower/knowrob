## Releasing KnowRob

### Versioning

The versioning of KnowRob follows the [Semantic Versioning](http://semver.org/) scheme.
The version number is stored in the file `package.xml` in the root directory of the package.

### Releasing a snapshot of the dev branch

To release a snapshot of the current development branch, follow these steps:

1. Make sure that the `package.xml` file contains the correct version number.
2. Commit all changes to the dev branch.
3. Go to the GitHub page of the KnowRob repository.
4. Click on the "Releases" tab.
5. Click on "Draft a new release".
6. Enter the tag version (e.g., `v1.0.0`) and the release title (e.g., `v1.0.0 snapshot`).
7. Enter a description of the changes since the last release.
8. Click on "Publish release".
9. The release is now available on the GitHub page of the KnowRob repository.
10. Change the version number in the `package.xml` file to the next minot version (e.g., `1.1.0`).

### Releasing a patch version

To release a patch version, follow these steps:

1. Create a new branch from the release branch with latest patches (e.g., `v1.0.2`).
2. Cherry-pick the commits that should be included in the patch release.
3. Follow the steps for releasing a snapshot of the dev branch.
