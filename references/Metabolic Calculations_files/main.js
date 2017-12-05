//
// --------------------------------------------------------------------------
// Default Javascript
// --------------------------------------------------------------------------
//
$(function() {
    // remove "no-js" from document
    $('.no-js').removeClass('no-js').addClass('js');
});


//
// --------------------------------------------------------------------------
// Search Module
// --------------------------------------------------------------------------
//
$(function() {
    $(window).click(function(target) {
        if ( !$(target.target).hasClass("search-form") && !$(target.target).parent().hasClass("search-form") && !$(target.target).parent().parent().hasClass("search-form") ) {
            $(".search-form form").removeClass("active");
        }
    });
    $(".search-icon").click(function() {
        if ( $(".search-form form").hasClass("active") ) {
            $(".search-form form").submit();
        }
        else {
            $(".search-form form").addClass("active");
            $(".search-form form input").focus();
        }
    });
});

//
// --------------------------------------------------------------------------
// Blog Module
// --------------------------------------------------------------------------
//
// toggle archive year
$(function() {
    $('.archive .toggle').each(function() { 
        $(this).click( function() {
            if ($(this).hasClass('year-show')) {
                $(this).removeClass('year-show').addClass('year-hide');
                $(this).attr('src', '/images/defaults/year-closed.png');
            } else {
                $(this).removeClass('year-hide').addClass('year-show');
                $(this).attr('src', '/images/defaults/year-opened.png');
            }
            $(this).nextAll('ul').slideToggle('fast');
        });
    });
});

// load DISQUS commenting
$(function() {
    $('#disqus_thread').each(function() {
        var shortname = $(this).data('shortname');
        if (shortname) {
            var s = document.createElement('script'); // s.async = true;
            s.type = 'text/javascript';
            s.src = 'http://' + shortname + '.disqus.com/count.js';
            (document.getElementsByTagName('HEAD')[0] || document.getElementsByTagName('BODY')[0]).appendChild(s);

            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; //  dsq.async = true;
            dsq.src = 'http://' + shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        }
    });
});



//
// --------------------------------------------------------------------------
// Forms
// --------------------------------------------------------------------------
//
$(function() {
    // validate and submit form
    $('.ajax-form').submit(function() {
        var form   = $(this);
        var submit = false;
        var params = $(this).serialize() + '&ajax=true';
        
        // check if form is valid
        $.ajax({
            dataType: 'json',
            type:     'POST',
            async:    false,
            data:     params,
            url:      '/includes/modules/form_validate.php',
            success: function(data) {
                // remove error messages
                $(form).find('.form-action-message, .form-error-message').remove();
                
                if (data.ok) {
                    submit = true;
                } else {
                    submit = false;
                    $(form).prepend('<div class="form-error-message">Please fill in the *Required fields.</div>');
                    
                    // update fields with error message
                    $.each(data, function(key, val) {
                        $('[data-field="' + key + '"]', form).append('<div class="form-action-message">' +  val + '</div>');
                    });
                    
                    // scroll to and focus on first error
                    if (data['focus_id']) {
                        $('html, body').animate({ scrollTop: $('[data-field="' + data['focus_id'] + '"]', form).offset().top - 80 }, 200);
                        $('[data-field="' + data['focus_id'] + '"]', form).find('select, input, textarea').get(data['focus_eq']).focus();
                    }
                }
            }
        });
        
        return submit;
    });
    
});



//
// --------------------------------------------------------------------------
// Placeholder Fields
// --------------------------------------------------------------------------
//
$(function() {

    // if placeholder functionality is not supported
    if (document.createElement("input").placeholder == undefined) {
    
        // add placeholder functionality to inputs
        $('[placeholder]').focus(function() {
            var input = $(this);
            if (input.val() == input.attr('placeholder')) {
                input.val('');
                input.removeClass('placeholder');
            }
        }).blur(function() {
            var input = $(this);
            if (input.val() == '' || input.val() == input.attr('placeholder')) {
                input.addClass('placeholder');
                input.val(input.attr('placeholder'));
            }
        }).blur();
        
        // clear placeholder text on form submit
        $('[placeholder]').parents('form').submit(function() {
            $(this).find('[placeholder]').each(function() {
                var input = $(this);
                if (input.val() == input.attr('placeholder')) {
                    input.val('');
                }
            })
        });
        
    }
});



//
// --------------------------------------------------------------------------
// Sign up for a Free Certification Resource Guide Block
// --------------------------------------------------------------------------
//
$(function() {
    $('.resource-guide-block form').submit(function() {
        var re = /\S+@\S+\.\S+/;
        var email = $('input[name="email"]', this).val();
        if (email == '') {
            alert('Please provide your email adress.');
            return false;
        }
        else if ( ! re.test(email)) {
            alert('Please provide a valide email adress.');
            return false;
        }
        return true;
    });
});



//
// --------------------------------------------------------------------------
// Responsive Tables
// --------------------------------------------------------------------------
//
$(function() {
    $('.content-main table').each(function() {
        $(this).wrap('<div class="table-container"></div>');
    });
});



//
// --------------------------------------------------------------------------
// FitVids
// --------------------------------------------------------------------------
//
$(function() {
    // ensure embedded videos are responsive
    $('.content-main').fitVids();
});



//
// --------------------------------------------------------------------------
// Mobile Navigation
// --------------------------------------------------------------------------
//
$(function() {
    $('.menu-button').click(function() {
        $('.off-canvas').toggleClass('active');
        $('.off-canvas-close').toggleClass('active');
    });
    $('.off-canvas-close').click(function() {
        $('.off-canvas').toggleClass('active');
        $('.off-canvas-close').toggleClass('active');
    });
    
    $('.side-navigation-button').click(function() {
        $(this).next().toggleClass('active');
    });
});



//
// --------------------------------------------------------------------------
// EIM Page
// --------------------------------------------------------------------------
//
$(function() {
    $('.expander').simpleexpand();
});



//
// --------------------------------------------------------------------------
// Home Page Slideshow
// --------------------------------------------------------------------------
//
(function($) {
    function HomepageSlideshow(element) {
        this.element         = element;
        this.slides          = $(element).find('.home-slide');
        this.pagination      = $(element).find('.dot');
        this.total_slides    = this.slides.length;
        this.last_slide      = this.slides.length - 1;
        this.current_slide   = -1;
        this.init();
    }
    
    // initiate the slideshow
    HomepageSlideshow.prototype.init = function() {
        var self = this;
        
        // add paging
        if (this.pagination.length) {
            self.pagination.click(function() {
                self.stop();
                self.selectSlide(self.pagination.index(this));
            });
        }
        
        // start
        self.selectSlide(0);
        self.play();
        
        // pause on rollover
        $(self.element).hover(function() {
            self.pause();
        }, function() {
            if (self.timer) {
                self.play();
            }
        });
    };
    
    // goto a specific slide
    HomepageSlideshow.prototype.selectSlide = function(index) {
        var self = this;
        if (index === self.current_slide) {
            return false;
        }
        if (index < 0) {
            index = self.last_slide;
        }
        if (index > self.last_slide) {
            index = 0;
        }
        
        $(self.slides[self.current_slide]).removeClass('active');
        $(self.pagination[self.current_slide]).removeClass('active');
        self.current_slide = index;
        $(self.slides[self.current_slide]).addClass('active');
        $(self.pagination[self.current_slide]).addClass('active');
    };
    
    // start the slideshow
    HomepageSlideshow.prototype.play = function() {
        var self = this;
        window.clearInterval(self.timer);
        self.timer = window.setInterval(function() {
            self.selectSlide(self.current_slide + 1);
        }, 5000);
    };
    
    // pause slideshow
    HomepageSlideshow.prototype.pause = function() {
        var self = this;
        if (self.timer) {
            window.clearInterval(self.timer);
        }
    };
    
    // stop slideshow
    HomepageSlideshow.prototype.stop = function() {
        var self = this;
        if (self.timer) {
            window.clearInterval(self.timer);
            self.timer = null;
        }
    };
    
    // plugin wrapper
    $.fn.homepageSlideshow = function() {
        return this.each(function() {
            if ( ! $.data(this, 'plugin_homepageSlideshow')) {
                $.data(this, 'plugin_homepageSlideshow', new HomepageSlideshow(this));
            }
        });
    };

})(jQuery);

$(function() {
    $('.home-slides').each(function() {
        $(this).homepageSlideshow();
    });
});



//
// --------------------------------------------------------------------------
// Google Analytics Event Tracking
// --------------------------------------------------------------------------
//
// This code is for Universal Analytics - analytics.js
// ie: <a href="http://smallboxweb.com/" data-ga-category="SmallBox" data-ga-action="click" data-ga-label="Text Link">Click Here</a>
// $(function() {
//     $('a[data-ga-category]').on('click', function(event) {
//         if (typeof(ga) !== 'undefined') {
//             var category = $(this).data('ga-category');
//             var action = $(this).data('ga-action');
//             if (category != undefined && action != undefined) {
//                 var defaultEvent = event.target;
//                 event.preventDefault();
                
//                 var params = {};
//                 params.hitType = 'event';
//                 params.eventCategory = category;
//                 params.eventAction = action;
//                 params.location = window.location.href;
                
//                 var href = $(this).attr('href');
//                 params.hitCallback = function() {
//                     document.location = href;
//                 }
                
//                 var label = $(this).data('ga-label');
//                 if (label !== undefined) {
//                     params.eventLabel = label;
//                 }
                
//                 ga('send', params);
//                 setTimeout(function() { document.location = href; }, 2000);
//             }
//         }
//     });
// });

//
// --------------------------------------------------------------------------
// Close Resource Guide Registration window
// --------------------------------------------------------------------------
//
function resourceGuideCallback() {
    window.location.href = '/aw-thankyou?zmsg=1';
}