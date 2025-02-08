figure;
title_size= 15;
font_size= 15;
text_size= 15;
%投影
m_proj('Equidistant cylindrical','lat',[15,27],'lon',[108,122.5]);
%使用m_tbase添加水深数据+填充
%[CS, CH] = m_tbase('contourf', [-5000:500:0],'edgecolor','none');
[CS,CH]=m_etopo2('contourf', [-5000:1000:0],'edgecolor','none');
%水深+等值线
%[CS, CH] = m_etopo2('contour', [-50 -100 -200], 'edgecolor','#C0C0C0', 'linewidth', 0.4);
%[CS, CH] = m_etopo2('contour', [100 -100 200], 'edgecolor','#828282', 'linewidth', 0.4);
%绘制高分辨率陆地和海岸线
m_gshhs_h('patch', [0.75 0.75 0.75], 'edgecolor', 'black', 'linewidth', 0.2);
m_grid('linestyle', 'none', 'linewidth',1.5, 'tickdir', 'out', ...
    'xaxisloc', 'bottom', 'yaisloc', 'right', 'fontsize', 10);
%设置颜色
colormap([m_colmap('blue', 256)]) %可选择其他的
%添加colorbar
h=colorbar('fontsize',10); 
%set(get(h,'title'),'string','meters');
% 颜色直接在图上修改

% 读取站点文件
hold on;
%Station = xlsread('all years.xlsx',1);
[num, text, row] =  xlsread('C:\\Users\\11485\\Desktop\\南海所\\毕业论文\\浮游软体动物分类学\\NSCS.csv',1);
station = (row(:,1));
lon = cell2mat(row(:,2));
lat = cell2mat(row(:,3));

% 添加站位和站位名称
hold on; 
 m_scatter(lon,lat,65,'^','MarkerEdgeColor','None','MarkerFaceColor','#FF0000',"linewi",1.6);%第3个是大小，第4个是形状,第6个是轮廓颜色.第8个是填充颜色,最后一个是线粗


