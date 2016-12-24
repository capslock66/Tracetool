<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0"
>

<xsl:template match='/Data'>
	<html>
		<head>
			<title>Tracetool sample </title>
			<style type="text/css">
				body { margin-top:10px; margin-bottom:25px; text-align:left; font-family: verdana, sans-serif; font-size: 80%; line-height: 1.45em; } 
				#block { margin:0px auto; width:600px; text-align:left; }
				p { padding-top: 0px; margin-top: 0px; }
				h1 { font-size: 120%; padding-bottom: 0px; margin-bottom: 0px; }
				h2 { font-size: 100%; margin-bottom: 0px; } 
                                ul li { color:brown }
			</style>
		</head>
		<body>
			Tracetool sample see <a href="http://www.codeproject.com/csharp/TraceTool.asp">TraceTool project</a>
                        <br/>
			<xsl:apply-templates select='Node' />
		</body>
	</html>
</xsl:template>

<xsl:template match='Node'>
   <!-- <xsl:number value="count(ancestor::*)" format="1 " />   -->
   <li>
      <xsl:value-of select='./text()'/>
      <xsl:if test="Col2/text() != '' ">
         <ul>
            Col2 : <xsl:value-of select='Col2/text()'/>
         </ul>
      </xsl:if>
      <ul>
      <xsl:apply-templates select='Node' />
      </ul>
   </li>
</xsl:template>


</xsl:stylesheet>