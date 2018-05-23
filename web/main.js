

// initial variables
var current_mode = "transit"
var current_measure = "T45"
var current_rural = "r_hide"
var current_dot = "d_none"
var canacc = "canacc_"
var access_data_name = canacc.concat(current_measure,"_",current_mode);
var all_access_data_layers = [
  "canacc_comp_transit","canacc_comp_car","canacc_comp_ratio",
  "canacc_T45_transit","canacc_T45_car","canacc_T45_ratio",
  "canacc_T60_transit","canacc_T60_car","canacc_T60_ratio",
  "canacc_T30_transit","canacc_T30_car","canacc_T30_ratio",
  "canacc_grav_transit","canacc_grav_car","canacc_grav_ratio"
]



// setting up the map :)
mapboxgl.accessToken = 'pk.eyJ1IjoiamVmZmFsbGVuIiwiYSI6ImNqaGlnMTFzMzI3b3QzZG50azltZmlsNXAifQ.U_UWmNAW_eSnwLJJJyNGow';
var map = new mapboxgl.Map({
    container: 'map', // div id
    style: 'mapbox://styles/jeffallen/cjgl7s02y001v2so0g098zg90',
    center: [-100, 49], // starting location
    zoom: 3, // starting zoom
    maxZoom: 14, // max zoom
    attributionControl: false,
});




// loading data to the map
map.on('load', function () {

  // add points for the 8 cities - including hiding with opacity when zoomed in
  map.addSource('citypoints', {
        "type": "geojson",
        "data": cities
    });
  map.addLayer({
      "id": "citypoints2",
      "source": "citypoints",
      "type": "circle",
      "paint": {
          "circle-radius": 10,
          "circle-color": "#ffffff",
          "circle-opacity": {
              type: 'exponential',
              base: 1,
              stops: [
                  [1, 1],
                  [6, 1],
                  [6.1, 0.0]
                ]
          }
      }
    });
  map.addLayer({
      "id": "citypoints",
      "source": "citypoints",
      "type": "circle",
      "paint": {
          "circle-radius": 7,
          "circle-color": "#ff2600",
          "circle-opacity": {
              type: 'exponential',
              base: 1,
              stops: [
                  [1, 1],
                  [6, 1],
                  [6.1, 0.0]
                ]
          }
      }
  });


});






// mode switching

var mode_c_ids = ['m_transit','m_car','m_ratio']
var mode_c_names = ['transit','car','ratio']

function mode_switch(mode_button_name) {
    current_mode = mode_button_name;
    access_data_name = canacc.concat(current_measure,"_",current_mode);
    console.log(access_data_name)

    // changing opacity to only view the selected layer
    for (var qq = 0; qq < all_access_data_layers.length; qq++) {
        if (all_access_data_layers[qq] == access_data_name) {
          map.setPaintProperty(all_access_data_layers[qq], 'fill-opacity', 1)
        }
        else {
          map.setPaintProperty(all_access_data_layers[qq], 'fill-opacity', 0)
        }
    }

    // set color of button
    for (var qq = 0; qq < 3; qq++) {
      if (mode_button_name == mode_c_names[qq]) {
        // changing opacity of buttons
        document.getElementById(mode_c_ids[qq]).style.opacity = '1.0';
        document.getElementById(mode_c_ids[qq]).style.color = 'red';
        }
      else {
        document.getElementById(mode_c_ids[qq]).style.opacity = '0.7';
        document.getElementById(mode_c_ids[qq]).style.color = 'black';
        }
    }

    // updating the legend values
    legend_array = map.getLayer(access_data_name).paint._values["fill-color"].value._parameters.stops
    if (current_mode == "ratio") {
      document.getElementById("legend_value_1").innerHTML = "<p>" + String(legend_array[0][0]) + " to " + String(legend_array[1][0]) + "</p>";
      document.getElementById("legend_value_2").innerHTML = "<p>" + String(legend_array[1][0]) + " to " + String(legend_array[2][0]) + "</p>";
      document.getElementById("legend_value_3").innerHTML = "<p>" + String(legend_array[2][0]) + " to " + String(legend_array[3][0]) + "</p>";
      document.getElementById("legend_value_4").innerHTML = "<p>" + String(legend_array[3][0]) + " to " + String(legend_array[3][0]) + "</p>";
      document.getElementById("legend_value_5").innerHTML = "<p>" + String(legend_array[4][0]) + " to " + "1.0" + "</p>";
    }
    else if (current_measure == "comp") {
      document.getElementById("legend_value_1").innerHTML = "<p>" + String(legend_array[0][0]) + " to " + String(legend_array[1][0]) + " (low) </p>";
      document.getElementById("legend_value_2").innerHTML = "<p>" + String(legend_array[1][0]) + " to " + String(legend_array[2][0]) + "</p>";
      document.getElementById("legend_value_3").innerHTML = "<p>" + String(legend_array[2][0]) + " to " + String(legend_array[3][0]) + "</p>";
      document.getElementById("legend_value_4").innerHTML = "<p>" + String(legend_array[3][0]) + " to " + String(legend_array[4][0]) + "</p>";
      document.getElementById("legend_value_5").innerHTML = "<p>" + String(legend_array[4][0]) + " to " + "1.0" + " (high) </p>";
    }
    else {
      document.getElementById("legend_value_1").innerHTML = "<p>" + legend_array[0][0].toLocaleString() + " to " + legend_array[1][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_2").innerHTML = "<p>" + legend_array[1][0].toLocaleString() + " to " + legend_array[2][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_3").innerHTML = "<p>" + legend_array[2][0].toLocaleString() + " to " + legend_array[3][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_4").innerHTML = "<p>" + legend_array[3][0].toLocaleString() + " to " + legend_array[4][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_5").innerHTML = "<p>" + legend_array[4][0].toLocaleString() + " and up" + " </p>";
    }
}





// access measure switching

var acc_c_ids = ['a_T60','a_T45','a_T30','a_grav','a_comp']
var acc_c_names = ['T60','T45','T30','grav','comp']

function measure_switch(access_button_name) {
    current_measure = access_button_name;
    access_data_name = canacc.concat(current_measure,"_",current_mode);
    console.log(access_data_name)

    // changing opacity to only view the selected layer
    for (var qq = 0; qq < all_access_data_layers.length; qq++) {
        if (all_access_data_layers[qq] == access_data_name) {
          map.setPaintProperty(all_access_data_layers[qq], 'fill-opacity', 1)
        }
        else {
          map.setPaintProperty(all_access_data_layers[qq], 'fill-opacity', 0)
        }
    }

    // set color of button
    for (var qq = 0; qq < 5; qq++) {
      if (access_button_name == acc_c_names[qq]) {
        // changing opacity of buttons
        document.getElementById(acc_c_ids[qq]).style.opacity = '1.0';
        document.getElementById(acc_c_ids[qq]).style.color = 'red';
        }
      else {
        document.getElementById(acc_c_ids[qq]).style.opacity = '0.7';
        document.getElementById(acc_c_ids[qq]).style.color = 'black';
        }
    }

    // updating the legend values
    legend_array = map.getLayer(access_data_name).paint._values["fill-color"].value._parameters.stops
    if (current_mode == "ratio") {
      document.getElementById("legend_value_1").innerHTML = "<p>" + String(legend_array[0][0]) + " to " + String(legend_array[1][0]) + "</p>";
      document.getElementById("legend_value_2").innerHTML = "<p>" + String(legend_array[1][0]) + " to " + String(legend_array[2][0]) + "</p>";
      document.getElementById("legend_value_3").innerHTML = "<p>" + String(legend_array[2][0]) + " to " + String(legend_array[3][0]) + "</p>";
      document.getElementById("legend_value_4").innerHTML = "<p>" + String(legend_array[3][0]) + " to " + String(legend_array[3][0]) + "</p>";
      document.getElementById("legend_value_5").innerHTML = "<p>" + String(legend_array[4][0]) + " to " + "1.0" + "</p>";
    }
    else if (current_measure == "comp") {
      document.getElementById("legend_value_1").innerHTML = "<p>" + String(legend_array[0][0]) + " to " + String(legend_array[1][0]) + " (low) </p>";
      document.getElementById("legend_value_2").innerHTML = "<p>" + String(legend_array[1][0]) + " to " + String(legend_array[2][0]) + "</p>";
      document.getElementById("legend_value_3").innerHTML = "<p>" + String(legend_array[2][0]) + " to " + String(legend_array[3][0]) + "</p>";
      document.getElementById("legend_value_4").innerHTML = "<p>" + String(legend_array[3][0]) + " to " + String(legend_array[4][0]) + "</p>";
      document.getElementById("legend_value_5").innerHTML = "<p>" + String(legend_array[4][0]) + " to " + "1.0" + " (high) </p>";
    }
    else {
      document.getElementById("legend_value_1").innerHTML = "<p>" + legend_array[0][0].toLocaleString() + " to " + legend_array[1][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_2").innerHTML = "<p>" + legend_array[1][0].toLocaleString() + " to " + legend_array[2][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_3").innerHTML = "<p>" + legend_array[2][0].toLocaleString() + " to " + legend_array[3][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_4").innerHTML = "<p>" + legend_array[3][0].toLocaleString() + " to " + legend_array[4][0].toLocaleString() + " </p>";
      document.getElementById("legend_value_5").innerHTML = "<p>" + legend_array[4][0].toLocaleString() + " and up" + " </p>";
    }
}




// switching between cities //

var city_c_ids = ['c_tor','c_van','c_cgy','c_edm','c_wpg','c_ott','c_mtl','c_que']
var city_c_names = ['Toronto','Vancouver','Calgary','Edmonton','Winnipeg','Ottawa','Montreal','Quebec City']

var current_city = ""
// zoom to city when point is clicked
map.on('click', 'citypoints', function (e) {
        current_city = e.features[0].properties.name
        map.flyTo({
        center: [
            e.features[0].geometry.coordinates[0],
            e.features[0].geometry.coordinates[1]],
        zoom: 9
        });
        var city_index = city_c_names.indexOf(current_city);
        document.getElementById(city_c_ids[city_index]).style.opacity = '1.0';
        document.getElementById(city_c_ids[city_index]).style.color = 'red';
    });

// Change the cursor to a pointer when the mouse is over the places layer.
map.on('mouseenter', 'citypoints', function (e) {
    map.getCanvas().style.cursor = 'pointer';
});
// Change it back to a pointer when it leaves.
map.on('mouseleave', 'citypoints', function () {
    map.getCanvas().style.cursor = '';
});

// zoom to city when button is clicked:
function city_switch(city_button_name) {
  current_city = city_button_name
  // querying city json to return coords to fly to
  cities.features.forEach(function(city) {
    if (city.properties.name == city_button_name) {
      map.flyTo({
      center: [
          city.geometry.coordinates[0],
          city.geometry.coordinates[1]],
      zoom: 9
      });
    }
  });

  // set color of button
  for (var qq = 0; qq < 8; qq++) {
    if (city_button_name == city_c_names[qq]) {
      // changing opacity of buttons
      document.getElementById(city_c_ids[qq]).style.opacity = '1.0';
      document.getElementById(city_c_ids[qq]).style.color = 'red';
      }
    else {
      document.getElementById(city_c_ids[qq]).style.opacity = '0.7';
      document.getElementById(city_c_ids[qq]).style.color = 'black';
      }
  }
}



// adding or removing rural (low population) areas
var rural_c_ids = ['r_hide','r_show']
// var city_c_names = ['Toronto','Vancouver','Calgary','Edmonton','Winnipeg','Ottawa','Montreal','Quebec City']

function rural_switch(rural_button_name) {
  current_rural = rural_button_name
  for (var qq = 0; qq < 2; qq++) {
    if (rural_button_name == rural_c_ids[qq]) {
      // changing opacity of buttons
      document.getElementById(rural_c_ids[qq]).style.opacity = '1.0';
      document.getElementById(rural_c_ids[qq]).style.color = 'red';
      }
    else {
      document.getElementById(rural_c_ids[qq]).style.opacity = '0.7';
      document.getElementById(rural_c_ids[qq]).style.color = 'black';
      }
  }

  if (current_rural == "r_hide") {
    map.setPaintProperty("canacc_popdensity", 'fill-opacity', {
          "base": 1,
          "type": "interval",
          "property": "pop_density",
          "stops": [[0, 1], [200, 0]]
        }
    )
  }
  else {
    map.setPaintProperty("canacc_popdensity", 'fill-opacity', {
          "base": 1,
          "type": "interval",
          "property": "pop_density",
          "stops": [[0, 0], [200, 0]]
        }
    )
  }
}




// adding or removing rural (low population) areas
var dot_c_ids = ['d_none','d_pop','d_unem','d_lico','d_reim']
var dot_n_ids = ['d_pop','d_unem','d_lico','d_reim']


function dot_switch(dot_button_name) {
  current_dot = dot_button_name
  console.log(current_dot)
  for (var qq = 0; qq < 5; qq++) {
    if (dot_button_name == dot_c_ids[qq]) {
      // changing opacity of buttons
      document.getElementById(dot_c_ids[qq]).style.opacity = '1.0';
      document.getElementById(dot_c_ids[qq]).style.color = 'red';
      }
    else {
      document.getElementById(dot_c_ids[qq]).style.opacity = '0.7';
      document.getElementById(dot_c_ids[qq]).style.color = 'black';
      }
  }

  if (current_dot != 'd_none') {
    for (var qq = 0; qq < 4; qq++) {
      if (dot_n_ids[qq] == current_dot) {
        map.setPaintProperty(dot_n_ids[qq], 'circle-opacity', 1)
      }
      else {
        map.setPaintProperty(dot_n_ids[qq], 'circle-opacity', 0)
      }
    }
  } else {
    for (var qq = 0; qq < 4; qq++) {
      map.setPaintProperty(dot_n_ids[qq], 'circle-opacity', 0)
    }
  }


}






// hovering over buttons red and not red options

function textred1(city_button_name_1) {
  document.getElementById(city_button_name_1).style.color = 'red';
}
function textred2(city_button_name_2) {
  var city_index = city_c_ids.indexOf(city_button_name_2);
  if (city_c_names[city_index] == current_city) {
    document.getElementById(city_button_name_2).style.color = 'red';
    document.getElementById(city_button_name_2).style.opacity = '1.0';
  }
  else {
    document.getElementById(city_button_name_2).style.color = 'black';
    document.getElementById(city_button_name_2).style.opacity = '0.7';
  }
}
function textred3(mode_button_name_3) {
  var mode_index = mode_c_ids.indexOf(mode_button_name_3);
  if (mode_c_names[mode_index] == current_mode) {
    document.getElementById(mode_button_name_3).style.color = 'red';
    document.getElementById(mode_button_name_3).style.opacity = '1.0';
  }
  else {
    document.getElementById(mode_button_name_3).style.color = 'black';
    document.getElementById(mode_button_name_3).style.opacity = '0.7';
  }
}
function textred4(acc_button_name_4) {
  var acc_index = acc_c_ids.indexOf(acc_button_name_4);
  if (acc_c_names[acc_index] == current_measure) {
    document.getElementById(acc_button_name_4).style.color = 'red';
    document.getElementById(acc_button_name_4).style.opacity = '1.0';
  }
  else {
    document.getElementById(acc_button_name_4).style.color = 'black';
    document.getElementById(acc_button_name_4).style.opacity = '0.7';
  }
}
function textred5(rural_button_name_5) {
  var rural_index = rural_c_ids.indexOf(rural_button_name_5);
  if (rural_c_ids[rural_index] == current_rural ) {
    document.getElementById(rural_button_name_5).style.color = 'red';
    document.getElementById(rural_button_name_5).style.opacity = '1.0';
  }
  else {
    document.getElementById(rural_button_name_5).style.color = 'black';
    document.getElementById(rural_button_name_5).style.opacity = '0.7';
  }
}
function textred6(dot_button_name_6) {
  var dot_index = dot_c_ids.indexOf(dot_button_name_6);
  if (dot_c_ids[dot_index] == current_dot ) {
    document.getElementById(dot_button_name_6).style.color = 'red';
    document.getElementById(dot_button_name_6).style.opacity = '1.0';
  }
  else {
    document.getElementById(dot_button_name_6).style.color = 'black';
    document.getElementById(dot_button_name_6).style.opacity = '0.7';
  }
}



// Add zoom and rotation controls to the map.
map.addControl(new mapboxgl.NavigationControl());

bar = new mapboxgl.ScaleControl({
    maxWidth: 100,
    unit: 'metric'
  });
map.addControl(bar);



// select dauid boundary when clicked in red
var selected_dauid = ""
map.on('click', function(e) {
    var features = map.queryRenderedFeatures(e.point, { layers: ['canacc_popdensity'] });
    if (!features.length) {
        return;
    }
    if (typeof map.getLayer('selected_da') !== "undefined" ){
        map.removeLayer('selected_da')
        map.removeSource('selected_da');
    }

    var feature = features[0];
    if (selected_dauid == feature.properties.dauid) {
      map.removeLayer('selected_da')
      // map.removeSource('selected_da');
      selected_dauid = ""
    }

    else {

      selected_dauid = feature.properties.dauid

      map.addSource('selected_da', {
          "type":"geojson",
          "data": feature.toJSON()
      });
      map.addLayer({
          "id": "selected_da",
          "type": "line",
          "source": "selected_da",
          "paint": {
            "line-color": "red",
            "line-width": 2
          }
      });
    }

});

// add a pop up
map.on('click', 'canacc_popdensity', function (e) {
       var popup_measure = current_measure + "_" + current_mode
       var coordinates = [e.lngLat.lng, e.lngLat.lat];

       if (current_mode == "ratio" || current_measure == "comp") {
         var description = "<p>dauid: " + e.features[0].properties["dauid"] +  "<br>population: " + (e.features[0].properties["population"]).toLocaleString() + "<br><b>value: " + (e.features[0].properties[popup_measure]).toLocaleString() + "</b></p>"
       }
       else {
          var description = "<p>dauid: " + e.features[0].properties["dauid"] +  "<br>population: " + (e.features[0].properties["population"]).toLocaleString() + "<br><b>value: " + (Math.round(e.features[0].properties[popup_measure])).toLocaleString() + "</b></p>"
       }

       while (Math.abs(e.lngLat.lng - coordinates[0]) > 180) {
           coordinates[0] += e.lngLat.lng > coordinates[0] ? 360 : -360;
       }

       if (selected_dauid != "") {
         new mapboxgl.Popup()
             .setLngLat(coordinates)
             .setHTML(description)
             .addTo(map);
       }
   });

////
