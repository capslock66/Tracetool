// TraceTable.CS
//
// construct a trace table
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information


// ReSharper disable ClassNeverInstantiated.Global
// ReSharper disable ConvertIfStatementToNullCoalescingExpression
// ReSharper disable ConvertIfStatementToConditionalTernaryExpression
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable IntroduceOptionalParameters.Global
// ReSharper disable FieldCanBeMadeReadOnly.Global
// ReSharper disable UnusedMethodReturnValue.Global
// ReSharper disable UnusedMember.Global
// ReSharper disable InlineOutVariableDeclaration
// ReSharper disable UseStringInterpolation
// ReSharper disable UseObjectOrCollectionInitializer
// ReSharper disable UseNullPropagation
// ReSharper disable MergeCastWithTypeCheck
// ReSharper disable UsePatternMatching
// ReSharper disable ArrangeAccessorOwnerBody

namespace TraceTool
{
    /// <summary>
    /// TraceTable class : construct a table of row to display in the viewer on a node.
    /// The table must be associated with a node. see TraceNodeEx.AddTable() and TraceToSend.SendTable()
    /// </summary>
    public class TraceTable
    {
        private readonly TMemberNode _members;
        private TMemberNode _currentRow;

        //----------------------------------------------------------------------

        /// <summary>
        /// create a table
        /// </summary>
        public TraceTable()
        {
            _members = new TMemberNode
            {
                ViewerKind = TraceConst.CST_VIEWER_TABLE
            };
            _currentRow = null;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// Add columns title : one or more columns titles separated by tabs
        /// </summary>
        /// <param name="colTitle">one or more columns titles separated by tabs. Can also be called several times to add titles</param>
        public void AddColumnTitle(string colTitle)
        {
            if (_members.Col1 == "")
                _members.Col1 = colTitle;
            else
                _members.Col1 = _members.Col1 + "\t" + colTitle;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// Add an empty row
        /// </summary>
        public void AddRow()
        {
            _currentRow = _members.Add("");
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// Add data to current row
        /// </summary>
        /// <param name="cell">one or more columns data separated by tabs. Can also be called several times to add cells</param>
        public void AddRowData(string cell)
        {
            if (_currentRow == null)
                AddRow();

            if (_currentRow.Col1 == "")
                _currentRow.Col1 = cell;
            else
                _currentRow.Col1 = _currentRow.Col1 + "\t" + cell;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// convert to members
        /// </summary>
        /// <param name="nodeMembers">target</param>
        internal void CopyToNodeMembers(TMemberNode nodeMembers)
        {
            var tableMembers = nodeMembers.Add(_members.Col1);
            tableMembers.ViewerKind = TraceConst.CST_VIEWER_TABLE;
            foreach (TMemberNode memberNode in _members.Members)
                tableMembers.Add(memberNode.Col1);
        }
    }
}       
