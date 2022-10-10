var plot;
var data = [];

const parse_ts = d3.timeParse("%Y-%m-%d %H:%M:%S");

d3.timeFormatDefaultLocale({
    "dateTime": "%A, der %e. %B %Y, %X",
    "date": "%d.%m.%Y",
    "time": "%H:%M:%S",
    "periods": ["AM", "PM"],
    "days": ["Sonntag", "Montag", "Dienstag", "Mittwoch",
             "Donnerstag", "Freitag", "Samstag"],
    "shortDays": ["So", "Mo", "Di", "Mi", "Do", "Fr", "Sa"],
    "months": ["Januar", "Februar", "MÃ¤¤rz", "April", "Mai", "Juni",
               "Juli", "August", "September", "Oktober", "November", "Dezember"],
    "shortMonths": ["Jan", "Feb", "Mrz", "Apr", "Mai", "Jun",
                    "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"]
});

const setData = (result) => {
    data = result;
    plot = document.getElementById("plot");
    plot.appendChild(Plot.plot({
        style: {
            background: "transparent"
        },
        x: {
            transform: d => parse_ts(d),
        },
        y: {
            grid: true
        },
        marks: [
            //Plot.ruleY([0]),
            Plot.line(result, {x: "ts", y: "temp"})
        ]
    }));
};

d3.csv("/data/heating.csv", d3.autoType).then(setData);

