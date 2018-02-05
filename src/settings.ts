/*
 *  Power BI Visualizations
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

module powerbi.extensibility.visual {
  "use strict";

  import DataViewObjectsParser = powerbi.extensibility.utils.dataview.DataViewObjectsParser;

 export class VisualSettings extends DataViewObjectsParser {
   // public rcv_script: rcv_scriptSettings = new rcv_scriptSettings();
   public mySettingsViz: mySettingsViz = new mySettingsViz();
   public mySettingsPlot: mySettingsPlot = new mySettingsPlot();
   public mySettingsFilter: mySettingsFilter = new mySettingsFilter();

   public mySettingsWeights: mySettingsWeights = new mySettingsWeights();
   public mySettingsKNN: mySettingsKNN = new mySettingsKNN();
   public mySettingsClust: mySettingsClust = new mySettingsClust();
   
  
    }


    
  
    export class mySettingsViz {
      public show: boolean = false;
     public distancesToLabeled: boolean = true;	
     public distancesToUnlabeled: boolean = true;	
     public clusteringResults: boolean = true;
     public origFeatures: boolean = true;	  
  }
  export class mySettingsWeights {
    public w1: string = "1";
    public w2: string = "1";
    public w3: string = "1";
    public w4: string = "1";
    public w5: string = "1";
    public w6: string = "1";
    public w7: string = "1";
    public w8: string = "1";
    public w9: string = "1";
    public toScale: boolean = true;
}
  export class mySettingsPlot {
    public show: boolean = false;
    public plotDim: string = "2";
  
  }
  export class mySettingsKNN {
    public compensate: boolean = true;
    public K: string = "3";
    public Kavg: string = "5";

}
export class mySettingsClust {
  public numClusters: string = "3";

}
export class mySettingsFilter {
  public rtype: string = "engagement";
  public numRows: string = "5";
  public keepOnly: string = "unlabeled";
  public sortBy: string = "medianNN";
  public show: boolean = false;

}

  export class rcv_scriptSettings {
   // undefined
    public provider     // undefined
    public source     }

}
