{$ifndef ver120}{$define d5}
  {$ifndef ver130}{$define d6}
    {$ifndef ver140}{$define d7}
      {$ifndef ver150}{$define d2005}
        {$ifndef ver170}{$define d2006}
          {$ifndef ver180}{$define d2007}
            {$ifndef ver185}{$define d2009}
              {$ifndef ver200}{$define d2010}
                {$ifndef ver210}{$define xe}
                  {$ifndef ver220}{$define xe2}
                    {$ifndef ver230}{$define xe3}
                      {$ifndef ver240}{$define xe4}
                        {$ifndef ver250}{$define xe5}
                          {$ifndef ver260}{$define xe6}
                            {$ifndef ver270}{$define xe7}
                              {$ifndef ver280}{$define xe8}
                                {$ifndef ver290}{$define d10seattle}
                                  {$ifndef ver300}{$define d10_1}
                                    {$ifndef ver310}{$define d10_2}
                                      {$ifndef ver320}{$define d10_3}
                                        {$ifndef ver330}{$define d10_4}
                                          {$ifndef ver340}{$define d11}
{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}{$endif}
{$ifdef d6}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_LIBRARY OFF}
  {$WARN UNIT_LIBRARY OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$ifdef d7}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$endif}
  {$define UnicodeOverloads}
{$endif}
{$R-}{$Q-}{$T-}{$D+}{$L+}
{$O+}
{$MINENUMSIZE 1}
{$ALIGN ON}
