// Função que lidará com filtros e busca na lista
$(document).ready(function () {
  $('#filter-Div').on("change keyup", function() {
    let regSearch = '';
    let regVend = '';
    regSearch = $('#0').val().trim().toLowerCase();
    regVend = $('#1').val();
    // Se não for selecionado nenhum, esconder tudo;
    if(regVend === "NENHUM"){
      $(".column").hide()
    }
    else {
      // Se selecionado um deles, caso já tenha digitado mais de três letras, começar a filtrar pela busca;
      if(regSearch.length > 2){
        $(".column").hide().filter(function() {
          let rtnData = "";

          if (regVend === "TODOS" || $(this).children("td:nth-child(3)").text().match(regVend)) {
              rtnData = (
                $(this).children("td:nth-child(2)").text().toLowerCase().match(regSearch) ||
                $(this).children("td:nth-child(4)").text().toLowerCase().match(regSearch)
         			 );
          }
          //console.log(regVend);
          //console.log(regSearch);
          //console.log(regSearch.length);
          //console.log();
          return rtnData;
        }).show();
      }
      // Se selecionado um deles, caso já não tenha digitado mais de três letras, mostrar todos;
      else{
        $(".column").hide().filter(function() {
          let rtnData = "";

          if (regVend === "TODOS" || $(this).children("td:nth-child(3)").text().match(regVend)) {
              rtnData = (
                $(this)
         			 );
          }
          return rtnData;
        }).show();
      }
    }
  })
})
