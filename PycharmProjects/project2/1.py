__author__ = 'Administrator'

import urllib
import urllib2
import re

class Tool:
    removeImg = re.compile('<img.*?>| {7}|')
    removeAddr = re.compile('<a.*?>|</a>')
    replaceLine=re.compile('<tr>|<div>|</div>|</p>')
    replaceTD = re.compile('<td>')
    replacePara = re.compile('<p.*?>')
    replaceBR=re.compile('<br><br>|<br>')
    removeExtraTag=re.compile('<.*?>')

    def replace(self, x):
        x=re.sub(self.removeImg, "",x)
        x=re.sub(self.removeAddr,"", x)
        x=re.sub(self.replaceLine,"\n",x)
        x=re.sub(self.replaceTD, "\t",x)
        x=re.sub(self.replacePara, "\n  ",x)
        x=re.sub(self.replaceBR, "\n", x)
        x=re.sub(self.removeExtraTag, "", x)
        return x.strip()


class bdtb:
    def __init__(self, baseUrl, seeLZ, floorTag):
        self.baseURL = baseUrl
        self.seeLZ = '?see_lz='+str(seeLZ)
        self.tool = Tool()
        self.file=None
        self.floor =1
        self.defaultTitle= "baidutieba"
        self.floorTag=floorTag
    def getPage(self, pageNum):
        try:
            url= self.baseURL +self.seeLZ+'&pn='+str(pageNum)
            request = urllib2.Request(url)
            response = urllib2.urlopen(request)
            return unicode(response.read(), 'utf8')
        except urllib2.URLError, e:
            if hasattr(e, "reason"):
                print e.reason
                return None
    def getTile(self, page):
        pattern = re.compile('<h1 class="core_title_txt.*?>(.*?)</h1>', re.S)
        result = re.search(pattern, page)
        if result:
            return result.group(1).strip()
        else:
            return None

    def getPageNum(selfself,page):
        pattern = re.compile('<li class="l_reply_num.*?</span>/*?<span.*>?(/*?)</span>', re.S)
        result = re.search(pattern, page)
        if result:
            return result.group(1).strip()
        else:
            return None

    def getContent(self,page):
        pattern = re.compile('<div id="post_content_.*?>(.*?)</div>', re.S)
        items = re.findall(pattern,page)
        contents=[]
        for item in items:
            content = "\n"+self.tool.replace(item)+"\n"
            contents.append(content.encode('utf-8'))
        return contents

    def setFileTitle(self, title):
        if title is not None:
            self.file=open(title+".txt", "w+")
        else:
            self.file=open(self.defaultTitle+".txt","w+")

    def writeData(self, contents):
        for item in contents:
            if self.floorTag == '1':
                floorLine = "\n"+str(self.floor)+u"-----------------------------------------------------------------------\n"
                self.file.write(floorLine)
            self.file.write(item)
            self.floor +=1

    def start(self ):
        indexPage = self.getPage(1)
        pageNum = self.getPageNum(indexPage)
        title=self.getTile(indexPage)
        self.setFileTitle(title)
        if pageNum ==None:
            print "URL does not exist"
            return
        try:
            print "there are "+str(pageNum)+" pages"
            for i in range(1, int(pageNum)+1):
                print "Writing "+str(i)+ "th page"
                page=self.getPage(i)
                contents = self.getContent(page)
                self.writeData(contents)
        except IOError, e:
            print "Error"+e.message
        finally:
            print "finish"

if __name__ == "__main__":
    print("Input page number:")
    baseURL = 'http://tieba.baidu.com/p/'+str(raw_input())
    seeLZ = raw_input("only LZ, input 1, or 0\n")
    floorTag=raw_input("write in, 1, or 0\n")
    bdtb=bdtb(baseURL, seeLZ, floorTag)
    bdtb.start()


