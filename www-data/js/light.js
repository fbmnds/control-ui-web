
const lightOn = () => {
    light = document.getElementById("light-form");
    light.innerHTML =
        '<label id="light-label" for="light-checkbox">' +
        '     Werkstatt Licht *AN*' +
        '</label>' +
        '<input type="checkbox" role="switch" id="light-checkbox" checked>';
}    

const lightOff = () => {
    light = document.getElementById("light-form");
    light.innerHTML =
        '<label id="light-label" for="light-checkbox">' +
        '     Werkstatt Licht *AUS*' +
        '</label>' +
        '<input type="checkbox" role="switch" id="light-checkbox">';
}    

const lightError = () => {
    light = document.getElementById("light-form");
    light.innerHTML =
        '<label id="light-label" for="light-checkbox">' +
        '     Werkstatt Licht *FEHLER*' +
        '</label>' +
        '<input type="checkbox" role="switch" id="light-checkbox">';
}
