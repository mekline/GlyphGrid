/**
 * jspsych-free-sort
 * plugin for drag-and-drop sorting of a collection of images
 * Josh de Leeuw
 *
 *
 *This version edited by: Miguel Salinas (masm@mit.edu)
 * 
 * documentation: https://github.com/jodeleeuw/jsPsych/wiki/jspsych-free-sort
 */

(function($) {
    jsPsych['free-sort'] = (function() {

        var plugin = {};

        plugin.create = function(params) {

            //params = jsPsych.pluginAPI.enforceArray(params, ['data']);

            var trials = new Array(params.stimuli.length);
            for (var i = 0; i < trials.length; i++) {
                trials[i] = {
                    "images": params.stimuli[i], // array of images to display
                    "stim_height": params.stim_height || 86,
                    "stim_width": params.stim_width || 86,
                    "timing_post_trial": (typeof params.timing_post_trial === 'undefined') ? 1000 : params.timing_post_trial,
                    "prompt": (typeof params.prompt === 'undefined') ? '' : params.prompt,
                    "prompt_location": params.prompt_location || "above",
                    "sort_area_width": params.sort_area_width || 800,
                    "sort_area_height": params.sort_area_height || 600,
                    "eventclips": (typeof params.eventclips === 'undefined') ? '' : params.eventclips[i],
                    "alien": params.alien,
                    "moreinstr": (typeof params.moreinstr === 'undefined') ? '' : params.moreinstr,
                    "eventpics": (typeof params.eventpics === 'undefined') ? 'blank.gif' : params.eventpics[i],
                    "eventpicside": params.eventpicside || 0,
                    "timer": params.timer || false,
                    "dragorclick": (typeof params.dragorclick === 'undefined') ? 'drag' : params.moreinstr
                };
            }
            return trials;
        };

        plugin.trial = function(display_element, trial) {
            
            // if any trial variables are functions
            // this evaluates the function and replaces
            // it with the output of the function
            trial = jsPsych.pluginAPI.normalizeTrialVariables(trial);

            var start_time = (new Date()).getTime();

            // check if there is a prompt and if it is shown above
            if (trial.prompt && trial.prompt_location == "above") {
                display_element.append(trial.prompt);
            }

            display_element.append($('<div>', {
                "id": "jspsych-free-sort-arena",
                "class": "jspsych-free-sort-arena",
                "css": {
                    "position": "relative",
                    //"left": "25px",
                    //"top": "25px",
                    "width": trial.sort_area_width,
                    "height": trial.sort_area_height
                }
            }));

            // check if prompt exists and if it is shown below
            if (trial.prompt && trial.prompt_location == "below") {
                display_element.append(trial.prompt);
            }


            timerHTML = '<div id="hex" style="height:2px;"><script src="/static/js/countdown.js"></script></div>';
            

            if (trial.timer) {
                display_element.append(timerHTML);
            };















            // store initial location data
            var init_locations = [];

            for (var i = 0; i < trial.images.length; i++) {
                //var coords = glyph_coordinate(trial.sort_area_width - trial.stim_width, trial.sort_area_height - trial.stim_height);
                var coords = {x:0, y:0};
                coords.x = (i%(trial.images.length/2))*100;
                if (i<(trial.images.length/2)) {
                    coords.y = 400;
                } 
                else{
                    coords.y = 500;
                };

                $("#jspsych-free-sort-arena").append($('<img>', {
                    "src": trial.images[i],
                    "class": "jspsych-free-sort-draggable",
                    "css": {
                        "position": "absolute",
                        "top": coords.y,
                        "left": coords.x,
                        "z-index": 1,
                        "height": 95,
                        "width": 95
                    }
                }));

                init_locations.push({
                    "src": trial.images[i],
                    "x": coords.x,
                    "y": coords.y
                });
            }


            var moves = [];

            if (trial.dragorclick == 'drag' || trial.dragorclick == 'both') {
                $('.jspsych-free-sort-draggable').draggable({
                    containment: "#jspsych-free-sort-arena",
                    scroll: false,
                    stack: ".jspsych-free-sort-draggable",
                    stop: function(event, ui) {
                        moves.push({
                            "src": event.target.src.split("/").slice(-1)[0],
                            "x": ui.position.left,
                            "y": ui.position.top
                        });
                    }
                });
            };


            if (trial.dragorclick == 'click' || trial.dragorclick == 'both') {
                $('.jspsych-free-sort-draggable').each(function() {
                    $(this).on('click',function() {
                        $(this).animate({
                            left: 657,
                            top: 57,
                            "z-index": 1,
                            stack: ".jspsych-free-sort-draggable"
                          }, {
                          duration: 500,
                          complete: function(event, ui) {
                                moves.push({
                                    "src": $(this).attr('src').split("/").slice(-1)[0],
                                    "x": $(this).position().left,
                                    "y": $(this).position().top
                                });
                            }
                        });
                    });
                });
            };




            $("#jspsych-free-sort-arena").append($('<img>', {
                "src": trial.eventpics,
                "class": "",
                "css": {
                    "position": "absolute",
                    "top": 0,
                    "left": 0,
                    "width": trial.eventpicside,
                    "height": trial.eventpicside
                }
            }));
            


            new_clippy = '<video id="Event" width="388" height="291" z-index=1 border=0 position="absolute" top="0" left="0" preload autoplay>'+
                '<source id="src_mpg" src="XXYYXX"  type=\'video/mp4; codecs="avc1.42E01E, mp4a.40.2"\'>';
            new_clippy = new_clippy.replace('XXYYXX', trial.eventclips);

            $("#jspsych-free-sort-arena").append(new_clippy)            

            $("#jspsych-free-sort-arena").append($('<img>', {
                "src": trial.alien,
                "class": "",
                "css": {
                    "position": "absolute",
                    "top": 0,
                    "left": 600,
                    "z-index": 0,
                    "height": 200,
                    "width": 200
                }
            }));


            
            $("#jspsych-free-sort-arena").append($(trial.moreinstr, {
                "class": "",
                "css": {
                    "position": "absolute",
                    "top": 320,
                    "left": 0,
                    "width": 800,
                    "border": 0
                }
            }));



            display_element.append($('<button>', {
                "id": "jspsych-free-sort-done-btn",
                "class": "jspsych-free-sort",
                "html": "Done",
                "click": function() {
                    var end_time = (new Date()).getTime();
                    var rt = end_time - start_time;
                    // gather data
                    // get final position of all objects
                    var final_locations = [];
                    $('.jspsych-free-sort-draggable').each(function() {
                        final_locations.push({
                            "src": $(this).attr('src'),
                            "x": $(this).css('left'),
                            "y": $(this).css('top')
                        });
                    });

                    jsPsych.data.write($.extend({}, {
                        "init_locations": JSON.stringify(init_locations),
                        "moves": JSON.stringify(moves),
                        "final_locations": JSON.stringify(final_locations),
                        "rt": rt
                    }, trial.data));

                    // advance to next part
                    display_element.html("");
                    if (trial.timing_post_trial > 0) {
                        setTimeout(function() {
                            jsPsych.finishTrial();
                        }, trial.timing_post_trial);
                    }
                    else {
                        jsPsych.finishTrial();
                    }
                }
            }));

        };

        // helper functions

        function glyph_coordinate(max_width, max_height) {
            var rnd_x = Math.floor(Math.random() * (max_width - 1));
            var rnd_y = Math.floor(Math.random() * (max_height - 1));

            return {
                x: rnd_x,
                y: rnd_y
            };
        }

        return plugin;
    })();
})(jQuery);
