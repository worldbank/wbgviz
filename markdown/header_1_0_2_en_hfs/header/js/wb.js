jQuery(document).ready(function($) {

	$("._search_submit").focus(function(){
		if($('._search_input').val() == 'Search') {
			$('._search_input').val("")
		}
	});

	$("._mobile_search_submit").focus(function(){
		if($('._mobile_search_input').val() == 'Search') {
			$('._mobile_search_input').val("")
		}
	});
	
	$('#wb_header_language .dropdown a.active').click(function(event) {
	    $('#wb_header_language .dropdown .dropdown-menu').toggle();
	    event.preventDefault();
	  })
	$('#mobile-menu-icon img').click(function(event) {
	    $('#mobile-menu-container').toggle();
	    event.preventDefault();
	})
	$('.header-mobile-languages .dropdown a.active').click(function(event) {
	    $('.header-mobile-languages .dropdown .dropdown-menu').toggle();
	    event.preventDefault();
	  })
	
	search_placeholder = $('#edit-qterm').val();
	$('#edit-qterm').focus(function(){
		if($(this).val() == search_placeholder){
			$(this).val('');
		}
		
	})
	$('#edit-qterm').blur(function(){
		if($(this).val() == ''){
			$(this).val(search_placeholder);
		}
	})

	$('.signup-updates form input[type="submit"]').click(function(){
		var email = $(this).parent().find('#fields_email').val();
		if(validateEmail(email)) {
			return true;
		} else {
			alert("The Email field is required.");
			$(this).parent().find('#fields_email').focus();
			return false;
		}
		
	});
	$('#newsletter form input[type="submit"]').click(function(){
		var email = $(this).parent().find('#fields_email').val();
		if(validateEmail(email)) {
			return true;
		} else {
			alert("The Email field is required.");
			$(this).parent().find('#fields_email').focus();
			return false;
		}
		
	});

// ******************************************************************

	$('._langswitch ._active, ._langswitch ._active_mobile').click(function(){
		if( $('._langswitch ul').css("display") == "none" ) {
			$('._langswitch ul').fadeIn();
			$('._mobile_menu').hide();
		} else {
			$('._langswitch ul').fadeOut();
		}
	});

	$('._langswitch ._mobile_icon').click(function(){
		if( $('._mobile_menu').css("display") == "none" ) {
			$('._mobile_menu').fadeIn();
			$('._langswitch ul').hide();
		} else {
			$('._mobile_menu').fadeOut();
		}
	});


	search_placeholder = $('._search_input').val();
	$('._search_input').focus(function(){
		if($(this).val() == search_placeholder){
			$(this).val('');
			$( this ).css('width','120px');
			$( this ).css('border','0');
			$('._search').css('box-shadow','0px 0px 1px #39beea');
		}
		
	})
	$('._search_input').blur(function(){
		if($(this).val() == ''){
			$(this).val(search_placeholder);
			$( this ).css('width','90px');
			$('._search').css('box-shadow','none');
		}
	})


});

function validateEmail(email) {
  var emailReg = /^([\w-\.]+@([\w-]+\.)+[\w-]{2,4})?$/;
  return emailReg.test(email);
};
