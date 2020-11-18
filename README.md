The wizirt package is up and running. To install it, use

    devtools::install_github('Pflegermeister/wizirt')

To get started using wizirt, first load it.

    library(wizirt)

To test if wizirt is working correctly, use irt\_report() with the
practice data. If this produces a report then wizirt is working on your
machine.

    data("responses")
    irt_report(data = responses[,-1])

You can see an example of how the output should look
[here](https://isaacpfleger.com/resources/dissertation/example_report.html).

You can more detailed instructions on using wizirt
[here](https://pflegermeister.github.io/wizirt/).
