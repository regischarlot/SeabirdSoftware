        ��  ��                  C	  P   T E X T   S C H E M A E X P L O R E R _ S U M M A R Y       0	        <html lang=en>
	<HEAD>
		<style type="text/css">
			<!--
				A			{font-family: Arial; color:"%ref_anchor%"; BACKGROUND-COLOR: transparent; font-weight: bold; font-size: 9pt; }
				a:visited	{color: %ref_anchor%; } 
				a:hover		{color: %ref_anchor%; background-color: %ref_anchorhoverbg%; cursor:default; } 
				TD			{font-family: Arial; color: black; font-size: 9pt; margin: auto; }
				TD.Header	{font-family: Arial; color: black; font-size: 12pt; font-weight: 500; margin: auto; padding: 10px; background-color: #F6FAFD;}
				TD.Item		{font-family: Arial; color: black; font-size: 9pt; margin: auto; padding: 10px;}
				TD.Line		{background-color:"#d0d0d0"; }
				.Header2	{font-family: Arial; color: black; font-size: 12pt; font-weight: bold; }
				
				TR.Small td				{text-align: Right; font-family: Arial; color: black; font-size: 8pt; }
				TR.Title td			{background: #9681B7; font-family: Arial; color: white; font-size: 8pt; font-weight: bold; text-align: center; }
				TR.Background td	{background: %colBG%;}
				TR.Highlight td		{background: #BBDB88;}
				TR.Alert td 		{background: #CF0226; color: white; font-weight: bold;}
				TD.HeaderColor				{background-color: #F6FAFD;}
			-->
		</style>
	</HEAD>
	<body BGCOLOR="white" marginwidth="0" marginheight="0" leftmargin="0" topmargin="0">
		<table border="0" cellpadding="0" cellspacing="0" width="100%">
			
			<tr>
				<td class="Header HeaderColor">%ref_ObjectType%&nbsp;<nospan class="Header2">%ref_ObjectName%</nospan></td>
				<td class="HeaderColor" style="text-align:right;"><img src="%ref_ApplicationPath%Images\Island.gif" border="0"></td>
			</tr>
			<tr><td colspan="2" height="0" class="Line" ></td></tr>

			<tr><td colspan="2">
				<table border="0" cellpadding="0" cellspacing="0">
					<tr><td class="Item" valign="top">Script</td><td class="Item" valign="top">Get <a href="%ref_FullScript%">full script</a> sorted by object dependency.</td></tr>
					<tr><td height="0"></td><td height="0" class="Line" ></td></tr>
					<tr><td class="Item" valign="top">Objects</td><td class="Item" valign="top"><table>%ref_Content%</table></td></tr>
					<tr><td height="0"></td><td height="0" class="Line" ></td></tr>
					<tr><td class="Item" valign="top">Help</td><td class="Item">%ref_Help%</td></tr>
				</table>
			</td></tr>

		</table>
	</body>
</html>
 d  P   T E X T   S C H E M A E X P L O R E R _ O B J E C T         0	        <html lang=en>
	<HEAD>
		<style type="text/css">
			<!--
				a 					{color: %ref_anchor%; font-size:8.0pt; font-family:"Courier New"; background-color: transparent; cursor:default; text-decoration: none; } 
				a:visited			{color: %ref_anchor%; } 
				a:hover				{color: %ref_anchor%; background-color: %ref_anchorhoverbg%; cursor:default; } 
				TD.Header			{font-family: Arial; color: black; font-size: 12pt; font-weight: 500; margin: auto; padding: 10px; background-color: #F6FAFD;}
				TD.HeaderColor		{background-color: #F6FAFD;}
				.Header2			{font-family: Arial; color: black; font-size: 12pt; font-weight: bold; }
				TD.Line				{background-color:"#d0d0d0"; }
				pre					{font-size:8.0pt; font-family:"Courier New"; white-space: pre-wrap; word-wrap: break-word; }
				span.comment		{color:#a0a0a0; }
			-->
		</style>
	</HEAD>
	<body BGCOLOR="white" marginwidth="0" marginheight="0" leftmargin="0" topmargin="0">
		<table border="0" cellpadding="0" cellspacing="0" width="100%">
			<tr>
				<td class="Header HeaderColor">%ref_ObjectType%&nbsp;<nospan class="Header2">%ref_ObjectName%</nospan></td>
				<td class="HeaderColor" style="text-align:right;"><img src="%ref_ApplicationPath%Images\Island.gif" border="0"></td>
			</tr>
			<tr><td colspan="2" height="0" class="Line" ></td></tr>
		</table>
		<pre>%ref_Content%</pre>
	</body>
</html>
B  L   T E X T   S C H E M A E X P L O R E R _ D A T A         0	        <html lang=en>
	<HEAD>
		<style type="text/css">
			<!--
				a 					{color: %ref_anchor%; font-size:8.0pt; font-family:"Courier New"; background-color: transparent; cursor:default; text-decoration: none; } 
				a:visited			{color: %ref_anchor%; } 
				a:hover				{color: %ref_anchor%; background-color: %ref_anchorhoverbg%; cursor:default; } 
				TD.Header			{font-family: Arial; color: black; font-size: 12pt; font-weight: 500; margin: auto; padding: 10px; background-color: #F6FAFD;}
				TD.HeaderColor		{background-color: #F6FAFD;}
				.Header2			{font-family: Arial; color: black; font-size: 12pt; font-weight: bold; }
				TD.Line				{background-color:"#d0d0d0"; }

				TD					{font-family: Arial; color: black; font-size: 8pt; cursor:default; background-color:white; vertical-align:top; text-align:left;}
				TR.header_red TD	{font-family: Arial; color: white; font-size: 8pt; font-weight: bold; cursor:default; background-color:#cf0225;}
				TR.header_purple TD	{font-family: Arial; color: white; font-size: 8pt; font-weight: bold; cursor:default; background-color:#9681b7;}
				TR.header_yellow TD	{font-family: Arial; color: white; font-size: 8pt; font-weight: bold; cursor:default; background-color:#f1af00;}
			-->
		</style>
	</HEAD>
	<body BGCOLOR="white" marginwidth="0" marginheight="0" leftmargin="0" topmargin="0">
		<table border="0" cellpadding="0" cellspacing="0" width="100%">
			<tr>
				<td class="Header HeaderColor">%ref_ObjectType%&nbsp;<nospan class="Header2">%ref_ObjectName%</nospan></td>
				<td class="HeaderColor" style="text-align:right;"><img src="%ref_ApplicationPath%Images\Island.gif" border="0"></td>
			</tr>
			<tr><td colspan="2" height="0" class="Line" ></td></tr>
		</table>

		<table border="0" cellpadding="0" cellspacing="4"><tr><td>%ref_Content%</td></tr></table>
	</body>
</html>
  P  T   T E X T   S C H E M A E X P L O R E R _ M E T A D A T A         0	        <html lang=en>
	<HEAD>
		<style type="text/css">
			<!--
				a 					{color: %ref_anchor%; font-size:8.0pt; font-family:"Courier New"; background-color: transparent; cursor:default; text-decoration: none; } 
				a:visited			{color: %ref_anchor%; } 
				a:hover				{color: %ref_anchor%; background-color: %ref_anchorhoverbg%; cursor:default; } 
				TD.Header			{font-family: Arial; color: black; font-size: 12pt; font-weight: 500; margin: auto; padding: 10px; background-color: #F6FAFD;}
				TD.HeaderColor		{background-color: #F6FAFD;}
				.Header2			{font-family: Arial; color: black; font-size: 12pt; font-weight: bold; }
				TD.Line				{background-color:"#d0d0d0"; }
				pre					{font-size:8.0pt; font-family:"Courier New"; white-space: pre-wrap; word-wrap: break-word; }
				span.comment		{color:#a0a0a0; }
			-->
		</style>
	</HEAD>
	<body BGCOLOR="white" marginwidth="0" marginheight="0" leftmargin="0" topmargin="0">
		<table border="0" cellpadding="0" cellspacing="0" width="100%">
			<tr>
				<td class="Header HeaderColor"><nospan class="Header2">%ref_ObjectName%</nospan></td>
				<td class="HeaderColor" style="text-align:right;"><img src="%ref_ApplicationPath%Images\Island.gif" border="0"></td>
			</tr>
			<tr><td colspan="2" height="0" class="Line" ></td></tr>
		</table>

		<pre>%ref_Content%</pre>
	</body>
</html>
