function(row, data) {
  /* palette generated in R with:
     library(pals)
     warmcool(length(seq(-1, 1, .1)))
  */
  var palette = [
    "#B40426", "#C53033", "#D55243", "#E36C55", "#ED8467", "#F3987A", "#F6AB8D", "#F6BBA2",
    "#F0C9B5", "#E8D4C9", "#DDDDDD", "#CFD8E9", "#BFD2F3", "#AFC9FB", "#9EBDFE", "#8CAEFC",
    "#7B9DF7", "#698AEF", "#5876E1", "#4961D1", "#3B4CC0"
  ];
  var colIdx = 0;

  function colorize() {
    var value = this.innerHTML.trim();
    if (colIdx === 0) {
      $(this).addClass("dt-right");
    } else if (/^[\d\.\-]+$/.test(value)) {
      value = parseFloat(value);
      var round = value < 0 ? Math.floor : Math.ceil;
      var colorIdx = round((value + 1) / 2 * 20);
      this.style.background = palette[colorIdx];
      this.style.fontWeight = "bold";
      if (Math.abs(value) > 0.6) {
        this.style.color = "white";
      }
      this.innerHTML = value.toFixed(2);
    }
    colIdx++;
  }

  $(row).find("td").each(colorize);
}
