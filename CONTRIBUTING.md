## Contributing to inspectEHR

inpsectEHR needs your help! Specifically, there are three main areas where contribution will be useful:

1. Documentation
2. Writing Tests
3. Expanding validation/verification framework

### Documentation

At the moment, documentation is only written by myself. Since I already know how the package works, I don't have a lot of feedback about whether or not the documentation is helpful. If you find something that is poorly explained (or missing) from the documentation, feel free to submit a pull request to update the documentation.

### Writing Tests

The current tests are quite limited. Please submit a pull request to add new unit tests that covers areas of functionality that are currently not covered.

### Expanding the framwork

Most of the package is written to my specific needs. If there are areas that are not being evaluated by the framework, feel free to write your own extensions and submit a pull request.

Please make sure you write documentation and unit tests for any new functionality you want to bring in.

You will also need to include a call to your new function inside `report()` so that it is run during the main report building.


Thank you so much!!
